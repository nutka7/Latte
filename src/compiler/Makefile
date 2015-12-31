all: binaries

modules:
	bnfc -haskell Latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x

binaries: modules
	ghc --make GenASM.hs
#	ghc --make TestLatte.hs -o TestLatte

clean:
	-rm -f *.hi *.o

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.*

.PHONY: all modules binaries clean distclean
