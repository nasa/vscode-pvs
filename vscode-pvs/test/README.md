## Regression tests
This folder contains pvs files and scripts used for the regression tests of VSCode-PVS. 

## How to run the regression test
1. Please edit pvsPath in `test.config` so that it points to the folder where PVS is installed
2. Please make sure the environment variable PVS_LIBRARY_PATH points to the folder where NASALib is installed, e.g., `export PVS_LIBRARY_PATH=~/PVS/pvs-8.1/nasalib` 
3. Launch pvs in server mode on port 23456 (./pvs -raw -port 23456)
4. Launch the regression test (npm run test)

## IMPORTANT NOTE
** DO NOT USE THE PVS FILES ** contained in the subfolders of this directory as baseline or reference for your developments: some of them contain intentional typecheck/parsing errors designed to test corner cases of VSCode-PVS.

