DASM	=  C:\run\dasm.exe
%.prg:
						$(DASM) $< -o$@ -l$*.lst

all:					movement_test.prg

shift_on_monitor.prg:				shift_on_monitor.s
movement_test.prg:			movement_test.s