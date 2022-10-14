DASM	=  'C:\run\dasm.exe'
%.prg:
						$(DASM) $< -o$@ -l$*.lst

all:					jumpingtest.prg

shift_on_monitor.prg:				shift_on_monitor.s
character_frame_switching_test.prg:		character_frame_switching_test.
jumpingtest.prg:		jumpingtest.s