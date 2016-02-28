
SOURCE=stubnets.cc

all: stubnets.so

test: stubnets.so
	yosys -q -l test1.log -m ./stubnets.so test.v -p "proc; stubnets"
	yosys -q -l test2.log -m ./stubnets.so test.v -p "proc; opt; stubnets"
	yosys -q -l test3.log -m ./stubnets.so test.v -p "proc; techmap; opt; stubnets"
	tail test1.log test2.log test3.log

stubnets.so: $(SOURCE)
	$(shell yosys-config --cxx --cxxflags --ldflags -o stubnets.so -shared $(SOURCE) --ldlibs )

clean:
	rm -f test1.log test2.log test3.log
	rm -f stubnets.so stubnets.d

