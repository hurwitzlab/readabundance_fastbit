#!/bin/bash

#PBS -W group_list=mbsulli
#PBS -q standard
#PBS -l jobtype=htc_only
#PBS -l select=1:ncpus=8:mem=23gb
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M kyclark@email.arizona.edu
#PBS -m ea

set -u

source /home/u20/kyclark/bin/common.sh

FILES=$(mktemp)

get_lines $FILES_LIST $FILES ${PBS_ARRAY_INDEX:=1} ${STEP_SIZE:=1}

NUM_FILES=$(lc $FILES)

echo Processing \"$NUM_FILES\" files

i=0
while read FILE; do
    let i++
    BASENAME=$(basename $FILE)
    printf "%5d: %s\n" $i $BASENAME

    COLS=$(head -1 $FILE | perl -F',' -nae 'print join ",", map { "f$_:int"} 1..$#F - 1')

    if [ -z "$COLS" ]; then
        echo Something went wrong trying to get columns
        break
    fi

    DIR=$OUT_DIR/$BASENAME 
    if [ -d $DIR ]; then
        rm -rf $DIR
    fi

    ardea -d $DIR -m \"rid:int,$COLS,pct:int\" -t $FILE
done < $FILES_LIST
