#!/bin/bash
contextFolder=$(pwd)
#echo pvs files are located in $contextFolder
cd "$(dirname "$0")"
node dist/prettify-pvs.js "-ctx" $contextFolder "$@"
# copying library files to the target folder
cp dist/prettify-pvs-code.js $contextFolder/out/scripts/prettify/prettify-pvs-code.js
cp dist/prettify-pvs-code.css $contextFolder/out/styles/prettify-pvs-code.css