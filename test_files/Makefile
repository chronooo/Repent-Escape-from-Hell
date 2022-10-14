#	usage:
#   type "make user", user being one of [archer, bern, taras]

default:					
										$(error No second argument found. Usage: make user, user being one of [archer, bern, taras])"

clean:
			rm *.prg *.lst 

archer: DASM = C:\run\dasm.exe	
archer: 								all
bern: DASM =  ~aycock/599.82/bin/dasm
bern: 									all
taras: DASM = dasm	
taras: 									all

%.prg:
										$(DASM) $< -o$@ -l$*.lst

all:									stubby.prg compiletest.prg compiletest2.prg custom_char_test.prg scrolling_test.prg	scrolling_test_bit_rotate.prg audio_test.prg shift_on_monitor.prg character_frame_switching_test.prg movement_test.prg health_test.prg

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
