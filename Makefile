.PHONY: all run clean

all: 6nimmt

6nimmt: bin obj
	gprbuild -Psix_nimmt

bin:
	mkdir -p bin

obj:
	mkdir -p obj

run: 6nimmt
	exec bin/6nimmt

pretty:
	gnatpp -Psix_nimmt

clean:
	gprclean -Psix_nimmt
	rm -rf bin obj
