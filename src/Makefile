all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	#latex DocInstant.tex; dvips DocInstant.dvi -o ../release/DocInstant.ps
	ghc --make Insc_jvm.hs -o ../insc_jvm
	ghc --make Insc_llvm.hs -o ../insc_llvm
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocInstant.ps
