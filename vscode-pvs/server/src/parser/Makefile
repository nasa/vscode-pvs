all: pvsParser ddlParser

pvsParser: download
	# creating java target
	@java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	@cp java/PvsParser.java out/java/ && cp java/DDL-MANIFEST.MF out/java/
	@cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && jar -cfm PvsParser.jar ./PVS-MANIFEST.MF *.class && cd ../..
	@-rm -r ../../out/parser/java/
	@mkdir ../../out/parser/java
	# completing installation
	@cp out/java/PvsParser.jar ../../out/parser/java
	@mv antlr-4.7.2-complete.jar ../../out/parser/java
	@make clean

javascript:
	java -jar antlr-4.7.2-complete.jar -Dlanguage=JavaScript PvsLanguage.g4 -o ./out/js

test:
	# java
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && cd ../..
	#@cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../server/test/sandbox/alaris2lnewmodes.pvs -gui
	cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../../../../../pvs-snapshots/PVS-6/nasalib/ACCoRD/line_solutions.pvs -gui

ddl: download
	# java
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlLanguage.g4 -o ./ddl/java/out
	cd ddl/java/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	cd ddl/java/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig DdlLanguage parseDDL ../../../examples/test.ddl -gui

ddl-embedding: download
	# java
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl/java/out
	cd ddl/java && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl/java/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	cd ddl/java/out && java -classpath ../../../lib/antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig DdlEmbedding parse ../../../examples/discrete_loop_hp.ddl -gui

ddl-parser: download
	# generating java target
	java -jar lib/antlr-4.7.2-complete.jar -Dlanguage=Java DdlEmbedding.g4 -o ./ddl/java/out
	cd ddl/java && cp DdlParser.java ./out && cp MANIFEST.MF ./out && cd ../..
	cd ddl/java/out && javac -Xlint -classpath ../../../lib/antlr-4.7.2-complete.jar:./ *.java && cd ../../..
	# creating ddlparser
	cd ddl/java/out && jar -cfm DdlParser.jar ./MANIFEST.MF *.class && rm *.class && cd ../../..
	# completing parser generation
	@-rm -r ../../out/parser/ddl
	@mkdir ../../out/parser/ddl
	cp ddl/java/out/DdlParser.jar ../../out/parser/java
	cp lib/antlr-4.7.2-complete.jar ../../out/parser/java
	cd ../../out/parser/java && java -jar DdlParser.jar ../../../src/parser/examples/discrete_loop_hp.ddl

	

download:
	@-mkdir lib
	cd lib && curl -O https://www.antlr.org/download/antlr-4.7.2-complete.jar

clean:
	@-rm -r out
	
