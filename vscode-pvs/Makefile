# Makefile for vscode-pvs
 # @author Paolo Masci
 # @date 2019.06.18
 # @copyright 
 # Copyright 2019 United States Government as represented by the Administrator 
 # of the National Aeronautics and Space Administration. All Rights Reserved.
 #
 # Disclaimers
 #
 # No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
 # WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
 # INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
 # WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
 # MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
 # INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
 # FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 # THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
 # CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
 # OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
 # OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
 # FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
 # REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
 # AND DISTRIBUTES IT "AS IS."
 #
 # Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
 # AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
 # SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
 # THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
 # EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
 # PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
 # SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
 # STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
 # PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
 # REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
 # TERMINATION OF THIS AGREEMENT.
 
all: install
	npm run compile
	make core
	make xterm-pvs

loc-server:
	find . -name '*.ts' ! -name '*.d.ts' -path '*server/src/*' | xargs wc -l

loc-client:
	find . -name '*.ts' ! -name '*.d.ts' -path '*client/src/*' | xargs wc -l

release: all install-vsce
	node_modules/vsce/out/vsce package
	#make resolutions
	#make resolutions

package:
	node_modules/vsce/out/vsce package

install-tsc:
	cd server && npm install typescript
	cd client && npm install typescript

install-vsce:
	npm install vsce

vsce:
	node_modules/vsce/out/vsce package

install: install-tsc
	npm install

test-proxy:
	npm run test-proxy
test-parser:
	npm run test-parser
test-find-declaration:
	npm run test-find-declaration
test-typechecker:
	npm run test-typechecker
test-prover:
	npm run test-prover
test-proof-script:
	npm run test-proof-script
test-workspace:
	npm run test-workspace
test-pvs-context:
	npm run test-pvs-context
test-pvs-language-server:
	npm run test-pvs-laguage-server
test-proof-explorer:
	npm run test-proof-explorer
test-tacticals:
	npm run test-tacticals
test-pvsio:
	npm run test-pvsio
test:
	npm run test
test-all: test

core:
	-mkdir server/out && mkdir server/out/core
	cd server/src/core/ && make

xterm-pvs:
	cd extra/xterm-pvs && make

pvs-parser-js:
	cd server/src/core/ && make pvs-parser-js

pvs-language:
	-mkdir server/out && mkdir server/out/core
	cd server/src/core/ && make pvs

clean: clean-test
	cd client && make clean && cd .. && cd server && make clean && cd ..
	rm -rf node_modules
	rm -f vscode-pvs-*.vsix

clean-test:
	-rm test/**/pvsbin/*
	-rm test/**/pvslog/*
	-rm test/**/.pvscontext

resolutions:
	npm install -no-save npm-force-resolutions
	node node_modules/npm-force-resolutions/index.js

audit:
	npm audit
	cd client && npm audit
	cd server && npm audit
	cd extra/xterm-pvs && npm audit
	cd extra/prettify-pvs && npm audit

.PHONY: test test-proxy test-parser test-find-declaration test-typechecker test-prover test-workspace test-pvs-context resolutions
