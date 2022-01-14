file="nasalib-files.txt"

build-nasalib-decls:
	# prints a table reporting all declarations in nasalib
	cd .. && java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-parser/javaTarget/out
	cd ../pvs-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java
	xargs < $(file) java -jar ../../../out/core/lib/PvsParser.jar > nasalib-decls.json

nasalib-decls:
	rsync nasalib-decls.json ../../../out/core/nasalib-utils/
	node ../../../out/core/nasalib-utils/process-decls