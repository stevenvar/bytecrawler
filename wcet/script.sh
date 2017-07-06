#!/bin/sh

CFLAGS="-mmcu=atmega103 -DUSB_VID=0x2341 -DUSB_PID=0x8036 -DF_CPU=16000000L -DARDUINO=105 -ffunction-sections -fdata-sections -g -Os -w -fno-exceptions"
LFLAGS="-Wl,-Os -Wl,--gc-sections"
rm "cyc.txt"
while read INST
do
    echo $INST
    sed -e '1 i\
#define OCAML_'$INST' 1
' interp.c > tests/wcet_$INST.c
    sed -i ".bak" -e 's/switch(opcode)/switch(OCAML_'$INST')/g' tests/wcet_$INST.c
    echo $INCLUDES
    avr-gcc -g -O2 -w -mmcu=atmega163 -DF_CPU=16000000L -DOCAML_VIRTUAL_ARCH=32 -I tests/lib/*.h tests/lib/*.c tests/wcet_$INST.c -o tests/wcet_$INST.avr
    NB=`boundt_avr -device=atmega163  -assert assert.txt tests/wcet_$INST.avr interp | Sed -n 's/Wcet:.*:interp:.*:\(.*\)/\1/p'`
    if [[ -z "$NB" ]]; then
       NB=-1
    fi
    echo $NB
    echo $INST:$NB >> cyc.txt
done < $1
