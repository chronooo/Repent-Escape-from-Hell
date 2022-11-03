USER=$(shell whoami)

ifeq "$(USER)" "bernardo.klein"
DASM=~aycock/599.82/bin/dasm
%.prg:
										$(DASM) $< -o$@ -l$*.lst
										cp $@ ~/www/hurrdurr.prg
										chmod 644 ~/www/hurrdurr.prg
										echo https://pages.cpsc.ucalgary.ca/~bernardo.klein/hurrdurr.prg
else ifeq "$(USER)" "genmcorp\archer"	
DASM:=C:\run\dasm.exe
%.prg:
										$(DASM) $< -o$@ -l$*.lst

else 
DASM=dasm
%.prg:
										$(DASM) $< -o$@ -l$*.lst
endif

all: 		titlescreentest.prg 

clean:
	- rm -f *.prg *.lst 

titlescreentest.prg:	titlescreentest.s
