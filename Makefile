all:
	stack ghc -- -O2 Main.hs
clean:
	rm -rf *.hi
	rm -rf *.exe
	rm -rf *.o