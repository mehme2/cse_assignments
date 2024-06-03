all: clean build run

build:
	@mkdir out
	@javac $$(find src | grep java) -sourcepath src -d out

run:
	@java -cp out Main

clean:
	@rm -rf out

docs:
	@rm -rf doc
	@javadoc -d doc -sourcepath src com.shop com.shop.device
