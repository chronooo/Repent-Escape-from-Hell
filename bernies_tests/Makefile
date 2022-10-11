DASM = ~aycock/599.82/bin/dasm

%.prg:
			$(DASM) $< -o$@ -l$*.lst
			cp $@ ~/www/hurrdurr.prg
			chmod 644 ~/www/hurrdurr.prg
			echo https://pages.cpsc.ucalgary.ca/~bernardo.klein/hurrdurr.prg

input:		inputtest.prg 

customg:	customgraphics.prg

titlescreen:	titlescreentest.prg


clean:
			rm *.prg *.lst 

inputtest.prg:	inputtest.s

customgraphics.prg:	customgraphics.s

titlescreentest.prg:	titlescreentest.s