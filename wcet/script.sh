#!/bin/sh

CFLAGS="-mmcu=atmega103 -DUSB_VID=0x2341 -DUSB_PID=0x8036 -DF_CPU=16000000L -DARDUINO=105 -ffunction-sections -fdata-sections -g -Os -w -fno-exceptions"
LFLAGS="-Wl,-Os -Wl,--gc-sections"
rm "cyc.txt"
while read LINE
do
    echo $LINE
    INST=`echo $LINE | cut -d' ' -f1`
    NLOOPS=`echo $LINE | cut -d' ' -f2`
    sed -e '1 i\
#define OCAML_'$INST' 1
' interp.c > tests/wcet_$INST.c
    sed -i ".bak" -e 's/switch(opcode)/switch(OCAML_'$INST')/g' tests/wcet_$INST.c
    echo $INCLUDES
    avr-gcc -g -O2 -w -mmcu=atmega163 -DF_CPU=16000000L -DOCAML_VIRTUAL_ARCH=32 -I tests/lib/*.h tests/lib/*.c tests/wcet_$INST.c -o tests/wcet_$INST.avr
    if [[ $NLOOPS -eq "0" ]]; then
	NB=`boundt_avr -device=atmega163 tests/wcet_$INST.avr interp | Sed -n 's/Wcet:.*:interp:.*:\(.*\)/\1/p'`
	echo $INST:$NB >> cyc.txt
	echo "ok"
    else
	for (( i=0; i <= $NLOOPS ; i++))
	do
	    cp assert.txt assert.$i.txt
	    sed -i ".bak" -e 's/XXX/'$i'/g' assert.$i.txt
	    echo $i
	    NB=`boundt_avr -device=atmega163 -assert assert.$i.txt tests/wcet_$INST.avr interp | sed -n 's/Wcet:.*:interp:.*:\(.*\)/\1/p'`
	    echo $INST "["$i"]":$NB >> cyc.txt
	done
	echo "nok"
    fi
done < $1
