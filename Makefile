all: compiler latc_x86
	 
compiler: src/compiler 
	    rm -rf compiler && cp -fr src/compiler . && cd compiler && $(MAKE) 
		 
latc_x86: 
	    cp src/latc_x86 .

clean: 
	    cd compiler && $(MAKE) clean 
		 
distclean: clean 
	    -rm -rf compiler latc_x86
		 
.PHONY: all clean distclean 
