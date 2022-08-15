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
	-cp CHANGELOG ../

loc-server:
	find . -name '*.ts' ! -name '*.d.ts' -path '*server/src/*' | xargs wc -l

loc-client:
	find . -name '*.ts' ! -name '*.d.ts' -path '*client/src/*' | xargs wc -l

release: all install-vsce audit
	node_modules/vsce/vsce package
	@echo "\033[0;32m** Release ready! To publish the release, type 'make publish' **\033[0m"

publish:
	node_modules/vsce/vsce publish

package:
	node_modules/vsce/vsce package

install-tsc:
	cd server && npm install typescript
	cd client && npm install typescript

install-vsce:
	npm install vsce

vsce:
	node_modules/vsce/out/vsce package

install: install-tsc
	npm install

build-nasalib-decls:
	cd server/src/common/nasalib-utils && make build-nasalib-decls

nasalib-decls:
	cd server/src/common/nasalib-utils && make nasalib-decls

test-proxy:
	npm run test-proxy
test-parser:
	npm run test-parser
test-find-declaration:
	npm run test-find-declaration
test-typechecker:
	npm run test-typechecker
test-prover:
ifdef PVS_LIBRARY_PATH
	npm run test-prover
else
	$(warning >> Warning: PVS_LIBRARY_PATH is not defined and NASALib is needed to run test-prover)
	npm run test-prover
endif
test-proof-script:
	npm run test-proof-script
test-workspace:
	npm run test-workspace
test-pvs-context:
ifdef PVS_LIBRARY_PATH
	npm run test-pvs-context
else
	$(warning >> Warning: PVS_LIBRARY_PATH is not defined and NASALib is needed to run test-pvs-context)
	npm run test-pvs-context
endif
test-pvs-language-server:
	npm run test-pvs-language-server
test-proof-explorer:
	npm run test-proof-explorer
test-tacticals:
	npm run test-tacticals
test-pvsio:
	npm run test-pvsio
test:
ifdef PVS_LIBRARY_PATH
	npm run test
else
	$(warning >> Warning: PVS_LIBRARY_PATH is not defined and NASALib is needed to run some of the test cases)
	npm run test
endif
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
	@echo "\033[0;32m** Auditing dependencies... **\033[0m"
	-npm audit fix
	-cd client && npm audit fix
	-cd server && npm audit fix
	-cd extra/xterm-pvs && npm audit fix
	-cd extra/prettify-pvs && npm audit fix
	@echo "\033[0;32m** Done with auditing! **\033[0m"

examples:
	@echo "\033[0;32m** Updating examples... **\033[0m"
	git submodule update
	@echo "\033[0;32m** Done with examples! **\033[0m"

.PHONY: test test-proxy test-parser test-find-declaration test-typechecker test-prover test-workspace test-pvs-context resolutions examples
