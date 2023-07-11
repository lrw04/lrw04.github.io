CHEZ=chez

all: bwog.ss
	$(CHEZ) --program bwog.ss .

.PHONY: all
