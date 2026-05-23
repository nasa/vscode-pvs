# How to generate the NASALib lookup table for NASALib
To generate a new NASALib lookup table to be included in the vscode-pvs distribution please follow these steps:
1. Install NASALib into PVS
2. Generate the list of NASALib files: `cd ~/PVS/pvs-8.1/nasalib && find "$PWD" -type f -name "*.pvs" > nasalib-files.txt `
3. Copy nasalib-files.txt to vscode-pvs/server/src/core/nasalib-utils
4. Use the makefile target build-nasalib-decls to build nasalib-decls.json file with the declaration table. The file is generated using ANTLR PvsParser (be patient it will take time to parse all the files): `make build-nasalib-decls`
5. Anonymize the paths indicates in the json file by removing the prefix and leaving only the folder name
6. Create nasalib-lookup-table.json: `make nasalib-decls`