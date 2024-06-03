all: clean build run

test: clean build random graph

build:
	@mkdir out
	@javac $$(find src | grep java) -sourcepath src -d out

graph:
	@java -Xint -cp out GUIVisualization random.txt

run:
	@java -cp out Main input.txt

random:
	@java -cp out RandomInputGenerator random.txt 100 100 100 100

clean:
	@rm -rf out

docs:
	@rm -rf doc
	@javadoc -d doc src/*
