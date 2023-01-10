#!/bin/bash
testFolder=$(pwd)/examples/test
cd "$(dirname "$0")"
node dist/pvs2latex-cli.js "$@"