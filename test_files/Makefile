DASM	=  'C:\run\dasm.exe'
%.prg:
						$(DASM) $< -o$@ -l$*.lst

all:					jumpingtest.prg

movement_test.prg:				movement_test.s
character_frame_switching_test.prg:		character_frame_switching_test.s
jumpingtest.prg:		jumpingtest.s