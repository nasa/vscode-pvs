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

# Ensure the base directory ends with a forward slash for consistent path joining
if (-not $RemoteBaseDir.EndsWith("/")) {
    $RemoteBaseDir += "/"
}

Write-Host "--- PVS File Sync Script ---"
Write-Host "PVS Lib Path: $($PvsLibPath)"
Write-Host "Remote User: $($RemoteUser)"
Write-Host "Remote IP: $($RemoteIP)"
Write-Host "Remote Base Directory: $($RemoteBaseDir)"
Write-Host "----------------------------"

# $PvsLibPath = "C:\Users\mmmos\Workspace\pvslib"
# $RemoteUser = "mmoscato"
# $RemoteIP = "192.168.12.203"
# $RemoteBaseDir = "/tmp/pvs-sync-server/c44f507e-9e02-4c55-a28c-bfb4e38488c1/lib/0289af408077e4f38178576b9392ef3fa7bdb84009586efb602a218e60760965/"

# Ensure the base directory ends with a forward slash for consistent path joining
if (-not $RemoteBaseDir.EndsWith("/")) {
    $RemoteBaseDir += "/"
}

# Get all .pvs files recursively from the current directory
$PvsFiles = Get-ChildItem -Path $PvsLibPath -Recurse -Include *.pvs,*.prf,pvs-strategies,pvs-attachments | Select-Object -ExpandProperty FullName

# Remove the base local path from the file's directory path
# Ensure a trailing backslash for consistent replacement if it's the root of the search


$PvsLibPathPattern = $PvsLibPath.Replace("\","\\")

# Loop through each .pvs file and copy it
foreach ($file in $PvsFiles) {
    # Calculate the relative path of the file
    $fileDirectory = (Get-Item $file).Directory.FullName
    Write-Host "fileDirectory $fileDirectory"
    $relativePath = $fileDirectory -replace $PvsLibPathPattern, ""
    Write-Host "relativePath $relativePath"

    # Ensure relative path uses forward slashes for Linux
    $relativePath = $relativePath.Replace("\", "/")

    # Construct the remote destination path for the directory
    $remoteDestinationDirectory = "$($RemoteBaseDir)$($relativePath)"

    if (-not $remoteDestinationDirectory.EndsWith("/")) {
        $remoteDestinationDirectory += "/"
    }

    # --- Create the remote directory if it doesn't exist ---
    # We use a single-quoted string for the remote command to avoid PowerShell's interpretation issues
    # and explicitly quote the path within the remote command.
    $sshCommand = "ssh $($RemoteUser)@$($RemoteIP) 'mkdir -p ""$($remoteDestinationDirectory)""'"
    #Write-Host "Executing remote mkdir: $sshCommand"
    Invoke-Expression $sshCommand

    # --- Copy the file using scp ---
    # For scp, the source path needs proper quoting for local Windows paths.
    # The destination needs the username@IP:path format.
    # Use ${} for robust variable expansion if there's a colon immediately after a variable,
    # though in this case, a subexpression $() is also clear.
    $remoteDestinationFile = "$($RemoteUser)@$($RemoteIP):$($remoteDestinationDirectory)$(Split-Path -Leaf $file)"
    $scpCommand = "scp `"$($file)`" `"$($remoteDestinationFile)`""
    #Write-Host "Executing SCP command: $scpCommand"
    Invoke-Expression $scpCommand

    Write-Host "Copied: $($file) to $($remoteDestinationFile)"
}

Write-Host "All .pvs files copied."