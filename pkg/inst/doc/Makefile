##
##  rtest
##  
##  utility 
##  create PDF document from dvi (usefull if R CMD INSTALL fails to build the pdf)
##  $Id$
##
all:       rtest.pdf clean
rtest.pdf: rtest.ps
	ps2pdf  -dEncodeColorImages=false -dColorImageFilter=/FlateEncode -dAutoRotatePages=/None rtest.ps
rtest.ps:  rtest.dvi
	dvips rtest
rtest.dvi: rtest.tex
									latex rtest
	latex rtest
clean:
	rm -f rtest.aux rtest.log rtest.toc
