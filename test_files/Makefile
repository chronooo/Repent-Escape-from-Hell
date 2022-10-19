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

all:									stubby.prg compiletest.prg compiletest2.prg custom_char_test.prg scrolling_test.prg scrolling_test_bit_rotate.prg audio_test.prg shift_on_monitor.prg character_frame_switching_test.prg movement_test.prg movement_test.prg health_test.prg titlescreentest.prg includetest.prg main_attempt.prg

clean:
	- rm -f *.prg *.lst 

stubby.prg:								stubby.s
compiletest.prg:						compiletest.s
compiletest2.prg:						compiletest2.s
custom_char_test.prg:					custom_char_test.s
scrolling_test.prg:						scrolling_test.s
scrolling_test_bit_rotate.prg:			scrolling_test_bit_rotate.s
audio_test.prg:							audio_test.s
shift_on_monitor.prg:					shift_on_monitor.s
character_frame_switching_test.prg:		character_frame_switching_test.s
movement_test.prg:						movement_test.s
health_test.prg:						health_test.s
titlescreentest.prg:					titlescreentest.s
includetest.prg:						includetest.s includetest1.s
main_attempt.prg:						main_attempt.s
