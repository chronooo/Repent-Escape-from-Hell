DASM	=  C:\run\dasm.exe
%.prg:
						dasm $< -o$@ -l$*.lst
						($DASM) $< -o$@ -l$*.lst

all:					stubby.prg compiletest.prg compiletest2.prg
all:					shift_on_monitor.prg

stubby.prg:				stubby.s
compiletest.prg:		compiletest.s
compiletest2.prg:		compiletest2.s
