
%.prg:
						dasm $< -o$@ -l$*.lst

all:					stubby.prg compiletest.prg compiletest2.prg

stubby.prg:				stubby.s
compiletest.prg:		compiletest.s
compiletest2.prg:		compiletest2.s