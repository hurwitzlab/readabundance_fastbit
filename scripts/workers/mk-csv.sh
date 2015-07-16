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
cd $CWD

source /home/u20/kyclark/bin/common.sh

TARGET=$(mktemp)

get_lines $FILE_LIST $TARGET ${PBS_ARRAY_INDEX:=1} $STEP_SIZE

NUM_FILES=$(lc $TARGET)

echo HOST \"$(hostname)\"
echo NUM_FILES \"$NUM_FILES\"

i=0
while read FILE; do
  let i++
  printf "%5d: %s\n" $i $FILE
  echo ./mk-csv.pl -i $IN_DIR -o $OUT_DIR -t $FILE
  ./mk-csv.pl -i $IN_DIR -o $OUT_DIR -t $FILE 
done < $TARGET

echo Done.
