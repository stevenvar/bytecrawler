#!/bin/bash

MMCU="atmega328p"
CFLAGS="-mmcu=$MMCU -DF_CPU=16000000L -DOCAML_VIRTUAL_ARCH=32 -gdwarf-3 -g3 -O1 -w -fno-exceptions"
LFLAGS="-Wl,-Os -Wl,--gc-sections"
rm -f "cyc.csv"
mkdir tmp
export BOUNDT_HELP=''
while read LINE
do
    INST=`echo $LINE | cut -d' ' -f1`
    NLOOPS=`echo $LINE | cut -d' ' -f2`
    sed -e '1 i\
#define OCAML_'$INST' 1
' interp.c > tmp/wcet_$INST.c
    cp -r lib tmp/lib
    sed -i ".bak" -e 's/switch(opcode)/switch(OCAML_'$INST')/g' tmp/wcet_$INST.c
    avr-gcc $CFLAGS -I lib/*.h lib/*.c tmp/wcet_$INST.c -o tmp/wcet_$INST.avr
    if [[ $NLOOPS -eq "0" ]]; then
	BNT=`boundt_avr -device=$MMCU tmp/wcet_$INST.avr interp 2>&1`
	if echo $BNT | grep -q 'Error' ; then
	    echo "$INST = error"
	    echo "$INST;-1" >> cyc.csv
	else
	    NB=`echo $BNT | sed -n 's/.*Wcet:.*:interp:.*:\(.*\)/\1/p'`
	    echo "$INST;$NB" >> cyc.csv
	    echo $INST = $NB
	fi
    else
	for (( i=0; i <= $NLOOPS ; i++))
	do
	    cp assert.txt assert.$i.txt
	    sed -i ".bak" -e 's/XXX/'$i'/g' assert.$i.txt
	    BNT=`boundt_avr -device=$MMCU -assert assert.$i.txt tmp/wcet_$INST.avr interp 2>&1`
	    if echo $BNT | grep -q 'Error' ; then
		echo "$INST [$i] = error"
		echo "${INST}_${i};-1" >> cyc.csv
	    else
		NB=`echo $BNT | sed -n 's/.*Wcet:.*:interp:.*:\(.*\)/\1/p'`
		echo "${INST}_${i};$NB" >> cyc.csv
		echo "$INST [$i] = $NB "
	    fi
	    rm assert.$i.txt
	    rm assert.$i.txt.bak

	done
    fi
    #rm tmp/wcet_$INST.c
    #rm tmp/wcet_$INST.avr
    #rm tmp/wcet_$INST.*.bak
done < $1
rm -rf tmp
