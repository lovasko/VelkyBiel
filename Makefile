HC=ghc
OPTS=-O3 -Wall

all: tw

tw: src/tw.hs
	$(HC) $(OPTS) -odir obj -hidir obj -o tw src/tw.hs

install:
	cp ./tw /usr/bin

clean:
	pwd
	rm -r ./tw ./obj

