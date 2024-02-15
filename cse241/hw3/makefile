default: clean build run

build:
	mkdir out
	javac src/Main.java -sourcepath src -d out

run:
	java -cp out Main

clean:
	rm -rf out

docs:
	rm -rf doc
	javadoc -d doc -sourcepath src com.gtu
