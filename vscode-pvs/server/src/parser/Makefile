all: download
	# java
	@java -jar antlr-4.7.2-complete.jar -Dlanguage=Java PvsLanguage.g4 -o ./out/java
	@cp java/PvsParser.java out/java/ && cp java/MANIFEST.MF out/java/
	@cd out/java && javac -Xlint -classpath ../../antlr-4.7.2-complete.jar:./ *.java && jar -cfm PvsParser.jar ./MANIFEST.MF *.class && cd ../..
	@-rm -r ../../out/parser/java/
	@mkdir ../../out/parser/java/
	@cp out/java/PvsParser.jar ../../out/parser/java
	# move antlr library
	@mv antlr-4.7.2-complete.jar ../../out/parser/java
	# remove targets
	@make clean

javascript:
	java -jar antlr-4.7.2-complete.jar -Dlanguage=JavaScript PvsLanguage.g4 -o ./out/js

test:
	cd ../../out/parser/java && java -classpath ../antlr-4.7.2-complete.jar:./PvsParser.jar org.antlr.v4.gui.TestRig PvsLanguage parse ../../../../server/test/sandbox/alaris2lnewmodes.pvs -gui

download:
	curl -O https://www.antlr.org/download/antlr-4.7.2-complete.jar

clean:
	-rm -r out
	
