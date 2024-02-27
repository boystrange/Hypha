
NAME = hypha

NULL =

DIRs = . Linearity DeadLock Session

FLAGS =
LFLAGS = -L/usr/lib

%.hs : %.x
	alex $<

%.hs %.info : %.y
	happy --info $<

.PHONY: $(NAME) clean
$(NAME) : Main.hs Lexer.hs Parser.hs
	ghc $(LFLAGS) --make $(FLAGS) -o $@ $<

clean:
	rm -f Lexer.hs Parser.hs Parser.info
	for i in $(DIRs); do rm -f $(NAME) $$i/*.o $$i/*.hi; done

