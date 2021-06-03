all:
	ghc -O2 --make Main.hs
clean:
	rm -rf *.hi
	rm -rf *.exe
	rm -rf *.o