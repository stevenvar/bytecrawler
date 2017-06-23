all : wcet.avr

wcet.avr : wcet.c
	avr-gcc -g -mmcu=atmega103 -DF_CPU=16000000L  $< -o $@

clean :
	rm *.avr
