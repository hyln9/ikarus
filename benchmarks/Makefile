
all: 
	date +"NOW: %Y-%m-%d %H:%M:%S" >tmp.time
	cat tmp.time >>log.time
	cat tmp.time >>log.error
	./benchall.ss 1>>log.time 2>>log.error
	rm -f z*.scm z*.tex tmp.time
