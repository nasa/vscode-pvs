param (
    [Parameter(Mandatory=$true, HelpMessage="Absolute path to the collection to be copied.")]
    [string]$PvsLibPath,

    [Parameter(Mandatory=$true, HelpMessage="Username for the remote SSH server.")]
    [string]$RemoteUser,

    [Parameter(Mandatory=$true, HelpMessage="IP address or hostname of the remote SSH server.")]
    [string]$RemoteIP,

    [Parameter(Mandatory=$true, HelpMessage="Base directory on the remote server where files will be copied.")]
    [string]$RemoteBaseDir
)

# --- Script Configuration ---
$ZipFileName = "pvslib_collection_$(Get-Date -Format 'yyyyMMddHHmmss').zip"
$TempZipDirectory = Join-Path $env:TEMP "PvsLibExport_$(New-Guid)" # Unique temp directory for staging
$LocalZipFilePath = Join-Path $TempZipDirectory $ZipFileName
$RemoteDestinationPath = "$($RemoteUser)@$($RemoteIP):$($RemoteBaseDir)"

# File types and specific files to include in the zip
# Note: 'pvs-strategies', 'pvs-patches', '.pvslib' are treated as specific filenames,
# while '*.pvs', '*.prf', '*.all' are treated as patterns.
$IncludePatterns = @(
    "*.pvs",
    "*.prf",
    "pvs-strategies",
    "pvs-patches",
    ".pvslib",
    "*.all"
)

Write-Host "--- PVS Library Collection Packaging and Transfer ---"
Write-Host "PVS Library Path (Input): $($PvsLibPath)"
Write-Host "Remote User: $($RemoteUser)"
Write-Host "Remote IP: $($RemoteIP)"
Write-Host "Remote Base Directory: $($RemoteBaseDir)"
Write-Host "Generated Zip File: $($ZipFileName)"
Write-Host "Local Temp Zip Path: $($LocalZipFilePath)"
Write-Host "Remote Destination: $($RemoteDestinationPath)"
Write-Host "Included Patterns: $($IncludePatterns -join ', ')"
Write-Host "---------------------------------------------------"

# --- 1. Validate PvsLibPath ---
if (-not (Test-Path $PvsLibPath -PathType Container)) {
    Write-Error "Error: PvsLibPath '$PvsLibPath' does not exist or is not a directory."
    exit 1
}

# --- 2. Stage files in a temporary directory ---
Write-Host "Staging files in temporary directory: $($TempZipDirectory)..."
try {
    # Create the temporary staging directory
    New-Item -ItemType Directory -Path $TempZipDirectory -Force | Out-Null

    # --- Prepare the PvsLibPath for robust comparison ---
    # This involves getting its canonical form, lowercasing the drive, and standardizing slashes.
    # The result will be used for string comparison to find the common prefix.
    $canonicalPvsLibPathOriginalCase = (Convert-Path $PvsLibPath)
    $pvsLibRootOriginalCase = [System.IO.Path]::GetPathRoot($canonicalPvsLibPathOriginalCase)
    
    $normalizedPvsLibPathForComparison = $canonicalPvsLibPathOriginalCase
    if (-not [string]::IsNullOrEmpty($pvsLibRootOriginalCase)) {
        $normalizedPvsLibPathForComparison = $pvsLibRootOriginalCase.ToLower() + $normalizedPvsLibPathForComparison.Substring($pvsLibRootOriginalCase.Length)
    }
    $normalizedPvsLibPathForComparison = $normalizedPvsLibPathForComparison.Replace('/', '\')
    # Ensure a trailing slash for consistent stripping later
    if (-not $normalizedPvsLibPathForComparison.EndsWith('\')) {
        $normalizedPvsLibPathForComparison += '\'
    }
    Write-Host "Normalized PvsLibPath for internal comparison: $($normalizedPvsLibPathForComparison)"


    # Recursively find and copy specified files/patterns
    $foundFiles = Get-ChildItem -Path $PvsLibPath -Recurse | Where-Object {
        $file = $_ # Current file/folder object

        # Check if it's a file and matches any of the include patterns
        if ($file.PSIsContainer -eq $false) { # If it's a file
            ($IncludePatterns | ForEach-Object {
                $pattern = $_
                if ($pattern.Contains('*')) { # Handle wildcard patterns
                    $file.Name -like $pattern
                } else { # Handle exact filenames (case-sensitive by default, use -ieq for case-insensitive)
                    $file.Name -eq $pattern
                }
            }) -contains $true # True if any pattern matches
        } else {
            $false # Don't include directories themselves, only their matching files
        }
    }

    if ($foundFiles.Count -eq 0) {
        Write-Warning "No files matching the specified patterns were found in '$PvsLibPath'. Aborting."
        Remove-Item $TempZipDirectory -Recurse -Force -ErrorAction SilentlyContinue
        exit 0
    }

    Write-Host "Found $($foundFiles.Count) files to include in the zip."

    foreach ($file in $foundFiles) {
        # --- Prepare the current file's FullName for robust comparison ---
        # Get its canonical form and normalize its drive letter and slashes.
        # The original $file.FullName will be used to extract the relative path segment.
        $canonicalFileFullNameOriginalCase = (Convert-Path $file.FullName)
        $fileRootOriginalCase = [System.IO.Path]::GetPathRoot($canonicalFileFullNameOriginalCase)

        $fileFullNameForComparison = $canonicalFileFullNameOriginalCase
        if (-not [string]::IsNullOrEmpty($fileRootOriginalCase)) {
            $fileFullNameForComparison = $fileRootOriginalCase.ToLower() + $fileFullNameForComparison.Substring($fileRootOriginalCase.Length)
        }
        $fileFullNameForComparison = $fileFullNameForComparison.Replace('/', '\')

        # --- Determine the relative path while preserving folder casing ---
        # Check if the normalized file path starts with the normalized base path.
        # This comparison is now robust against drive letter casing and slash differences.
        if ($fileFullNameForComparison.StartsWith($normalizedPvsLibPathForComparison, [System.StringComparison]::Ordinal)) {
            # The length of the common prefix that we need to remove from the ORIGINAL path.
            # This is the length of our 'normalizedPvsLibPathForComparison' string.
            $lengthToRemove = $normalizedPvsLibPathForComparison.Length
            
            # Extract the relative path from the *original* canonical path.
            # This ensures that folder casing is preserved.
            $relativePath = $canonicalFileFullNameOriginalCase.Substring($lengthToRemove)
        } else {
            Write-Warning "File path '$($file.FullName)' does not appear to be a child of '$($PvsLibPath)' after normalization. Skipping this file."
            continue # Skip to the next file if the path isn't a child as expected
        }

        # Determine the parent relative path for creating directories in staging:
        # Handle edge case where file is directly in PvsLibPath (no subfolders)
        if (-not $relativePath.Contains('\') -and -not $relativePath.Contains('/')) {
            $parentRelativePath = "" # File is directly in the root, no subfolder structure
        } else {
            # Split-Path operates on $relativePath, which now retains original folder casing.
            $parentRelativePath = Split-Path -Path $relativePath -Parent
        }

        # Construct the full destination directory path in the temp staging directory
        $destinationDirectoryInTemp = Join-Path $TempZipDirectory $parentRelativePath
        
        # Create necessary parent directories in the temp staging area
        New-Item -ItemType Directory -Path $destinationDirectoryInTemp -Force | Out-Null
        
        # Copy the file to the staging directory.
        # Use $file.FullName (from Get-ChildItem) for the source, as it's the actual path on disk.
        Copy-Item -Path $file.FullName -Destination (Join-Path $destinationDirectoryInTemp $file.Name) -Force
        Write-Host "  Staged: $($file.FullName) -> $($destinationDirectoryInTemp)\$($file.Name)"
    }
    Write-Host "Files staged successfully."

} catch {
    Write-Error "Error staging files: $($_.Exception.Message)"
    Write-Error "Error details (for debugging): $($_.Exception | Format-List -Force)" # Provide more details
    Remove-Item $TempZipDirectory -Recurse -Force -ErrorAction SilentlyContinue
    exit 1
}

# --- 3. Create the Zip file ---
Write-Host "Creating zip file: $($LocalZipFilePath)..."
try {
    # It's crucial to zip the *contents* of the temp directory, not the directory itself,
    # if you want the root of the zip to be the collected files.
    Compress-Archive -Path "$($TempZipDirectory)\*" -DestinationPath $LocalZipFilePath -Force
    Write-Host "Zip file created successfully."
} catch {
    Write-Error "Error creating zip file: $($_.Exception.Message)"
    Write-Error "Error details (for debugging): $($_.Exception | Format-List -Force)"
    Remove-Item $TempZipDirectory -Recurse -Force -ErrorAction SilentlyContinue
    exit 1
}

# --- 4. Copy the Zip file using SCP -v ---
Write-Host "Copying '$($LocalZipFilePath)' to '$($RemoteDestinationPath)' using scp -v..."
try {
    # @M3 existence of the remote base dir is responsibility of the caller
    # # Ensure the remote base directory exists on the server
    # # Use -p to ensure parent directories are created
    # $sshMkdirCommand = "ssh $($RemoteUser)@$($RemoteIP) 'mkdir -p ""$($RemoteBaseDir)""'"
    # Write-Host "Executing remote mkdir: $sshMkdirCommand"
    # Invoke-Expression $sshMkdirCommand
    #
    # if ($LASTEXITCODE -ne 0) {
    #     Write-Error "Failed to ensure remote base directory exists. SSH command exited with code $LASTEXITCODE."
    #     Remove-Item $LocalZipFilePath -ErrorAction SilentlyContinue
    #     Remove-Item $TempZipDirectory -Recurse -Force -ErrorAction SilentlyContinue
    #     exit 1
    # }

    # Use scp -v for verbose output
    # Ensure proper quoting for local path
    $scpCommand = "scp -v `"$($LocalZipFilePath)`" `"$($RemoteDestinationPath)/$($ZipFileName)`""
    Write-Host "Executing SCP command: $scpCommand"
    Invoke-Expression $scpCommand

    if ($LASTEXITCODE -ne 0) {
        Write-Error "SCP failed! Command exited with code $LASTEXITCODE. Check SSH/SCP output above for details."
        # The -v output from scp will be in the console, so just point to it.
        exit 1
    }

    Write-Host "Zip file copied successfully to remote server."

} catch {
    Write-Error "Error during SCP transfer: $($_.Exception.Message)"
    Write-Error "Error details (for debugging): $($_.Exception | Format-List -Force)"
    exit 1
} finally {
    # --- 5. Clean up temporary files ---
    Write-Host "Cleaning up temporary files..."
    try {
        if (Test-Path $LocalZipFilePath) {
            Remove-Item $LocalZipFilePath -ErrorAction SilentlyContinue
        }
        if (Test-Path $TempZipDirectory) {
            Remove-Item $TempZipDirectory -Recurse -Force -ErrorAction SilentlyContinue
        }
        Write-Host "Temporary files cleaned up."
    } catch {
        Write-Warning "Failed to clean up temporary files: $($_.Exception.Message)"
    }
}

Write-Host "--- Script finished. ---"