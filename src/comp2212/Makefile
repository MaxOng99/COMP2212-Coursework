DEPEND += Tokens.hs Grammar.hs TypeChecker.hs SplEvaluator.hs

all: $(DEPEND) Main
	@mv Main myinterpreter

Main: $(DEPEND) Main.hs
	ghc Main.hs 

Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y

Tokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x

clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info