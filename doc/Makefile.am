

#dist_doc_DATA=doc/ikarus-users-guide.pdf
EXTRA_DIST = ikarus-users-guide.tex ikarus-users-guide.bib ikarus-users-guide.pdf

TEX=xelatex -output-driver="xdvipdfmx -q -E" 

ikarus-users-guide.pdf: ikarus-users-guide.tex ikarus-users-guide.bib
	$(TEX) ikarus-users-guide
	makeindex ikarus-users-guide
	bibtex ikarus-users-guide
	$(TEX) ikarus-users-guide
	$(TEX) ikarus-users-guide

all: ikarus-users-guide.pdf

clean: 
	rm -f *.aux *.log *.toc *.out *.idx *.ind *.ilg
