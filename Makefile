CHEZ=chez

all: bwog.ss
	$(CHEZ) --script bwog.ss blog

.PHONY: all
