all: compiler lib latc_x86
	 
compiler:
	    rm -rf compiler && cp -fr src/compiler . && cd compiler && $(MAKE)

lib:
		rm -rf lib && cp -fr src/lib . && clang -c lib/runtime.c -o lib/runtime.o
		 
latc_x86: 
	    cp src/latc_x86 .

clean: 
	    cd compiler && $(MAKE) clean 
		 
distclean: clean 
	    -rm -rf compiler latc_x86
		 
.PHONY: all clean distclean compiler lib
