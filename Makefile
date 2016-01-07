all: compiler lib latc_x86
	 
compiler:
	    rm -rf compiler && cp -fr src/compiler . && cd compiler && $(MAKE)

lib:
		rm -rf lib && cp -fr src/lib . && gcc -c -m32 lib/runtime.c -o lib/runtime.o && gcc -c -m32 lib/main.c -o lib/main.o
		 
latc_x86: 
	    cp src/latc_x86 .

clean: 
	    cd compiler && $(MAKE) clean 
		 
distclean: clean 
	    -rm -rf compiler latc_x86 lib
		 
.PHONY: all clean distclean compiler lib
