# DASM	=  'C:\run\dasm.exe' - ARCHER WAY
# %.prg:
# 						$(DASM) $< -o$@ -l$*.lst

# all:					character_frame_switching_test.prg

# shift_on_monitor.prg:				shift_on_monitor.s
# character_frame_switching_test.prg:		character_frame_switching_test.s


%.prg:
										dasm $< -o$@ -l$*.lst

all:									stubby.prg compiletest.prg compiletest2.prg custom_char_test.prg scrolling_test.prg	scrolling_test_bit_rotate.prg audio_test.prg

stubby.prg:								stubby.s
compiletest.prg:						compiletest.s
compiletest2.prg:						compiletest2.s
custom_char_test.prg:					custom_char_test.s
scrolling_test.prg:						scrolling_test.s
scrolling_test_bit_rotate.prg:			scrolling_test_bit_rotate.s
audio_test.prg:							audio_test.s
shift_on_monitor.prg:					shift_on_monitor.s
character_frame_switching_test.prg:		character_frame_switching_test.s