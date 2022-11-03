USER=$(shell whoami)

ifeq "$(USER)" "taras"
DASM=dasm
%.prg:
										$(DASM) $< -o$@ -l$*.lst
else ifeq "$(USER)" "genmcorp\archer"	
DASM:=C:\run\dasm.exe
%.prg:
										$(DASM) $< -o$@ -l$*.lst

else 
DASM=~aycock/599.82/bin/dasm
%.prg:
										$(DASM) $< -o$@ -l$*.lst
										cp $@ ~/www/hurrdurr.prg
										chmod 644 ~/www/hurrdurr.prg
										echo https://pages.cpsc.ucalgary.ca/~bernardo.klein/hurrdurr.prg
endif

all: 		zx0Compress.prg

clean:
	- rm -f *.prg *.lst 

zx0Compress.prg:	zx0Compress.s zx0-6502.s
