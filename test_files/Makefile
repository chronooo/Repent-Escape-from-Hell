
%.prg:
						dasm $< -o$@ -l$*.lst

all:					stubby.prg compiletest.prg compiletest2.prg custom_char_test.prg

stubby.prg:				stubby.s
compiletest.prg:		compiletest.s
compiletest2.prg:		compiletest2.s
custom_char_test.prg:	custom_char_test.s