
%.prg:
								dasm $< -o$@ -l$*.lst

all:							stubby.prg compiletest.prg compiletest2.prg custom_char_test.prg scrolling_test.prg	scrolling_test_bit_rotate.prg

stubby.prg:						stubby.s
compiletest.prg:				compiletest.s
compiletest2.prg:				compiletest2.s
custom_char_test.prg:			custom_char_test.s
scrolling_test.prg:				scrolling_test.s
scrolling_test_bit_rotate.prg:	scrolling_test_bit_rotate.s