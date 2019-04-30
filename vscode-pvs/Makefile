all:
	npm install
	node_modules/vsce/out/vsce package

install:
	npm install

clean:
	cd client && make clean && cd .. && cd server && make clean && cd ..
	rm -r node_modules
	rm vscode-pvs-*.vsix
