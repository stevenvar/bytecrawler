#!/bin/bash

MMCU="atmega2560"
CFLAGS="-I /usr/local/include/omicrob/vm -mmcu=$MMCU -DF_CPU=16000000L -DOCAML_VIRTUAL_ARCH=16 -gdwarf-3 -g3 -O1 -w -fno-exceptions"
LFLAGS="-Wl,-Os -Wl,--gc-sections"
LOOPCOST=37
rm -f "cyc.csv"
mkdir tmp
export BOUNDT_HELP=''
while read LINE
do
    INST=`echo $LINE | cut -d' ' -f1`
    NLOOPS=`echo $LINE | cut -d' ' -f2`
    sed -e '1 i\
    #define OCAML_'$INST' 0
 ' interp.c > tmp/wcet_$INST.c
    sed -i ".bak" -e 's/switch(opcode)/switch(OCAML_'$INST')/g' tmp/wcet_$INST.c
    avr-gcc $CFLAGS  tmp/wcet_$INST.c -o tmp/wcet_$INST.avr
    if [[ $NLOOPS -eq "0" ]]; then
	BNT=`boundt_avr -device=$MMCU tmp/wcet_$INST.avr main 2>&1`
	if echo $BNT | grep -q 'Error' ; then
	    echo "$INST = error"
	    echo "$INST;-1" >> cyc.csv
	else
	NB=`boundt_avr -device=$MMCU tmp/wcet_$INST.avr main 2>&1 | sed -n 's/.*Wcet:.*:main:.*:\(.*\)/\1/p'`
	NB=`echo "$NB + $LOOPCOST" | bc` # 21 is the measured cost of the switch (in cycles)
	    echo "$INST;$NB" >> cyc.csv
	    echo $INST = $NB
	fi
    else
	for (( i=0; i <= $NLOOPS ; i++))
	do
	    cp assert.txt assert.$i.txt
	    sed -i ".bak" -e 's/XXX/'$i'/g' assert.$i.txt
	    BNT=`boundt_avr -device=$MMCU -assert assert.$i.txt tmp/wcet_$INST.avr main 2>&1`
	    if echo $BNT | grep -q 'Error' ; then
		echo "$INST [$i] = error"
		echo "${INST}_${i};-1" >> cyc.csv
	    else
		NB=`echo $BNT | sed -n 's/.*Wcet:.*:main:.*:\(.*\)/\1/p'`
		NB=`echo "$NB + $LOOPCOST" | bc` # 21 is the measured cost of the switch (in cycles)
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
# rm -rf tmp
