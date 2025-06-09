all:
	npm install
	make build
	make webpack
	make copy

build:
	npm run build

webpack:
	npm run webpack

copy:
	rsync bundle/xterm-pvs.min.js ../../client/out/extra/

clean:
	rm -rf node_modules
	rm package-lock.json

