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

all: 									specific_RLE.prg

clean:
	- rm -f *.prg *.lst 

specific_RLE.prg:						specific_RLE.s