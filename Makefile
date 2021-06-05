all:
	stack ghc -- -O2 Main.hs
clean:
	rm -rf *.hi
	rm -rf *.exe
	rm -rf *.o
	rm -rf ./Module/*.hi
	rm -rf ./Module/*.o
	rm -rf ./Test/*.md