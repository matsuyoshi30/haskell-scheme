all:
	stack ghc Main.hs

run: Main
	./Main

test:
	stack runghc test.hs

clean:
	rm Main *.hi *.o
