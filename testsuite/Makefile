TGT = runtests

all: $(TGT)

$(TGT): runtests.hs 
	ghc --make runtests.hs -o runtests -i../src

clean:
	rm -rf *.o *.hi *~ 


