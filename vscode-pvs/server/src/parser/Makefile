all:
	make pvs-parser
	make ddl-parser

pvs: download
	# java
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-parser/javaTarget/out
	cd pvs-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../test/sandbox/sq.pvs -gui
	# cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../examples/test.pvs -gui
	# cd pvs-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../test/alaris/alaris2lnewmodes.pvs -gui

pvs-parser: download
	# generating java target
	@java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./pvs-parser/javaTarget/out
	@cd pvs-parser/javaTarget && cp PvsParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	@cd pvs-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	# creating pvsparser
	cd pvs-parser/javaTarget/out && jar -cfm PvsParser.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in parser/lib
	@-mkdir ../../out/parser/lib
	cp pvs-parser/javaTarget/out/PvsParser.jar ../../out/parser/lib
	cp lib/antlr-4.7.2-complete.jar ../../out/parser/lib
	@-rm -r pvs-parser/javaTarget/out
	@-rm -r lib
	# testing parser
	cd ../../out/parser/lib && java -jar PvsParser.jar ../../../test/sandbox/sq.pvs -test

pvs-parser-js: download
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=JavaScript PvsLanguage.g4 -o ./pvs-parser/jsTarget/out

test: download
	# java
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && cd ../..
	#@cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../server/test/sandbox/alaris2lnewmodes.pvs -gui
	cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../../../../pvs-snapshots/PVS-6/nasalib/ACCoRD/line_solutions.pvs -gui

ddl: download
	# java
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	cd ddl-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig DdlEmbedding testDDL ../../../examples/test.hp -gui

ddl-embedding: download
	# java
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	cd ddl-parser/javaTarget/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig DdlEmbedding parse ../../../examples/testhp.hpvs -gui

ddl-parser: download
	# generating java target
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl-parser/javaTarget/out
	cd ddl-parser/javaTarget && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl-parser/javaTarget/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	# creating ddlparser
	cd ddl-parser/javaTarget/out && jar -cfm DdlParser.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing installation in parser/lib
	@-mkdir ../../out/parser/lib
	cp ddl-parser/javaTarget/out/DdlParser.jar ../../out/parser/lib
	cp lib/antlr-4.7.2-complete.jar ../../out/parser/lib
	@-rm -r ddl-parser/javaTarget/out
	@-rm -r lib
	# testing parser
	cd ../../out/parser/lib && java -jar DdlParser.jar ../../../src/parser/examples/testhp.hpvs -test
	cd ../../out/parser/lib && java -jar DdlParser.jar ../../../src/parser/examples/testhp.hpvs -out ../../../src/parser/examples/testhp.pvs

	

download:
	@-mkdir lib
	cd lib && curl -O https://www.antlr.org/download/antlr-4.7.2-complete.jar

clean:
	@-rm -r ddl-parser/javaTarget/out
	@-rm -r pvs-parser/javaTarget/out
	@-rm -r pvs-parser/jsTarget/out
	@-rm -r lib
	
