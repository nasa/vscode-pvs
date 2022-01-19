all:
	make pvs-parser
	# make ddl-parser
	# make pvs-typechecker

test-pvs-parser: download
	# java
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-parser/javaTarget/out
	cd pvs-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	# cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../test/sandbox/sq.pvs -gui
	cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage theory ../../../examples/helloworld.pvs -gui
	# cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../examples/test.pvs -gui
	# cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../test/alaris/alaris2lnewmodes.pvs -gui

ls-pvs-files:
	# prints the absolute path of all pvs files in the current folder and all subfolders
	find "$(pwd)" . -type f \( -name "*.pvs" \) | grep "^/" > pvs-files.txt

build-nasalib-decls:
	cd nasalib-utils && make build-nasalib-decls

nasalib-decls:
	cd nasalib-utils && make nasalib-decls

nasalib-theories-by-folder:
	cd nasalib-utils && make nasalib-theories-by-folder

pvs-parser: download
	# generating java target
	@java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-parser/javaTarget/out
	@cd pvs-parser/javaTarget && cp *.java ./out && cp MANIFEST.MF ./out && cd ../..
	@cd pvs-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	# creating pvs-parser
	cd pvs-parser/javaTarget/out && jar -cfm PvsParser.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in core/lib
	@-mkdir ../../out/core/lib
	cp pvs-parser/javaTarget/out/PvsParser.jar ../../out/core/lib
	cp lib/antlr-4.9.3-complete.jar ../../out/core/lib
	@-rm -r pvs-parser/javaTarget/out
	# @-rm -r lib
	# testing parser
	#cd ../../out/core/lib && java -jar PvsParser.jar ../../../test/sandbox/sq.pvs -test
	cd ../../out/core/lib && java -jar PvsParser.jar ../../../src/core/examples/test.pvs

pvs-outline-parser: download
	# generating js target
	@java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=JavaScript PvsLanguageOutline.g4 -o ./pvs-outline-parser/jsTarget/out

pvs-typechecker: download
	# generating java target
	@java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-typechecker/javaTarget/out
	@cd pvs-typechecker/javaTarget && cp *.java ./out && cp ../../pvs-parser/javaTarget/*.java ./out && cp MANIFEST.MF ./out && cd ../..
	@cd pvs-typechecker/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	# creating pvs-typechecker
	cd pvs-typechecker/javaTarget/out && jar -cfm PvsTypechecker.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in core/lib
	@-mkdir ../../out/core/lib
	cp pvs-typechecker/javaTarget/out/PvsTypechecker.jar ../../out/core/lib
	cp lib/antlr-4.9.3-complete.jar ../../out/core/lib
	# @-rm -r pvs-typechecker/javaTarget/out
	# @-rm -r lib
	# testing typechecker
	# cd ../../out/core/lib && java -jar PvsTypechecker.jar ../../../test/sandbox/sq.pvs -test
	cd ../../out/core/lib && java -jar PvsTypechecker.jar ../../../src/core/examples/test-typechecker.pvs -outline

pvs-parser-js: download
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=JavaScript PvsLanguage.g4 -o ../../out/core/pvs-parser/jsTarget/out

test: download
	# java
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../..
	cd out/java && java -classpath ../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../server/test/sandbox/alaris2lnewmodes.pvs -gui
	# cd out/java && java -classpath ../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../../../../pvs-snapshots/PVS-6/nasalib/ACCoRD/line_solutions.pvs -gui

ddl: download
	# java
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	cd ddl-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig DdlEmbedding testDDL ../../../examples/test.hp -gui

test-ddl-parser: download
	# java
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	cd ddl-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig DdlEmbedding parse ../../../examples/testhp.hpvs -gui

ddl-parser: download
	# generating java target
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	# creating ddlparser
	cd ddl-parser/javaTarget/out && jar -cfm DdlParser.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in parser/lib
	@-mkdir ../../out/core/lib
	cp ddl-parser/javaTarget/out/DdlParser.jar ../../out/core/lib
	cp lib/antlr-4.9.3-complete.jar ../../out/core/lib
	@-rm -r ddl-parser/javaTarget/out
	# @-rm -r lib
	# testing parser
	cd ../../out/core/lib && java -jar DdlParser.jar ../../../src/core/examples/testhp.hpvs -test
	cd ../../out/core/lib && java -jar DdlParser.jar ../../../src/core/examples/testhp.hpvs -out ../../../src/core/examples/testhp.pvs

ddl-pretty-printer: download
	# generating java target
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java DdlPrettyPrinter.g4 -o ./ddl-pretty-printer/javaTarget/out
	cd ddl-pretty-printer/javaTarget && cp DdlPrettyPrinter.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl-pretty-printer/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../../..
	# creating ddl-pretty-printer
	cd ddl-pretty-printer/javaTarget/out && jar -cfm DdlPrettyPrinter.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in parser/lib
	@-mkdir ../../out/core/lib
	cp ddl-pretty-printer/javaTarget/out/DdlPrettyPrinter.jar ../../out/core/lib
	cp lib/antlr-4.9.3-complete.jar ../../out/core/lib
	@-rm -r ddl-pretty-printer/javaTarget/out
	# @-rm -r lib
	# testing pretty-printer
	cd ../../out/core/lib && java -jar DdlPrettyPrinter.jar ../../../src/core/examples/test.ddl
	# cd ../../out/core/lib && java -jar DdlPrettyPrinter.jar -translate "NOT ALLRUNS(SEQ(ANY(x,DLRANDOM),ANY(y,DLRANDOM)),val(x)=val(y))"

test-ddl-pretty-printer: download
	java -jar lib/antlr-4.9.3-complete.jar -Dlanguage=Java DdlPrettyPrinter.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../lib/antlr-4.9.3-complete.jar:./ *.java && cd ../..
	cd out/java && java -classpath ../../lib/antlr-4.9.3-complete.jar:./ org.antlr.v4.gui.TestRig DdlPrettyPrinter prettyprintDdl ../../examples/test.ddl -gui


download:
	@-mkdir lib
	-cd lib && $(if $(shell which curl), curl, wget) -O https://www.antlr.org/download/antlr-4.9.3-complete.jar
	

clean:
	@-rm -r ddl-parser/javaTarget/out
	@-rm -r pvs-parser/javaTarget/out
	@-rm -r pvs-parser/jsTarget/out
	@-rm -r lib
	@-rm ddl-parser/javaTarget/*.class
	@-rm ddl-pretty-printer/javaTarget/*.class
	@-rm pvs-parser/javaTarget/*.class
	
.PHONY: ddl-pretty-printer ddl-parser pvs-parser test-pvs-parser
