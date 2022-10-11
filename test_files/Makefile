DASM	=  C:\run\dasm.exe
%.prg:
						$(DASM) $< -o$@ -l$*.lst

all:					animation_test.prg

shift_on_monitor.prg:				shift_on_monitor.s
animation_test.prg:			animation_test.s