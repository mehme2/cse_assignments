all: clean build run

build:
	@mkdir out
	@javac -Xlint $$(find src | grep java) -sourcepath src -d out

run:
	@java -cp out Main

clean:
	@rm -rf out

docs:
	@rm -rf doc
	@javadoc -d doc src/*
