##
##  RUnit
##  
##  utility 
##  create PDF document from dvi (usefull if R CMD INSTALL fails to build the pdf)
##  $Id: Makefile 13377 2010-08-30 07:51:02Z Mario $
##
all:       RUnit.pdf clean
RUnit.pdf: RUnit.ps
	ps2pdf  -dEncodeColorImages=false -dColorImageFilter=/FlateEncode -dAutoRotatePages=/None RUnit.ps
RUnit.ps:  RUnit.dvi
	dvips RUnit
RUnit.dvi: RUnit.tex
									latex RUnit
	latex RUnit
clean:
	rm -f RUnit.aux RUnit.log RUnit.toc
