all: download
	# creating java target
	@java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	@cp java/PvsParser.java out/java/ && cp java/MANIFEST.MF out/java/
	@cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && jar -cfm PvsParser.jar ./MANIFEST.MF *.class && cd ../..
	@-rm -r ../../out/parser/java/
	@mkdir ../../out/parser/java/
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

ddl:
	# java
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsDDL.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && cd ../..
	cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsDDL parseDDL ../../examples/test.ddl -gui

ddllanguage:
	# java
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguageDDL.g4 -o ./out/java
	cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && cd ../..
	cd out/java && java -classpath ../../antlr-4.7.2-complete.jar:./ org.antlr.v4.gui.TestRig PvsLanguageDDL parse ../../examples/discrete_loop_hp.ddl -gui

download:
	curl -O https://www.antlr.org/download/antlr-4.7.2-complete.jar

clean:
	@-rm -r out
	
