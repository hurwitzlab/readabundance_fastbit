#!/bin/bash

set -u

export CWD=$PWD
export STEP_SIZE=2000
export IN_DIR=/rsgrps/bhurwitz/kyclark/pov/data/read-modes/
export OUT_DIR=/rsgrps/bhurwitz/kyclark/pov/data/fastbit/csv

#export IN_DIR=/rsgrps/bhurwitz/iychoi/metagenomics/bonnie_sample/mode
#export OUT_DIR=/rsgrps/bhurwitz/kyclark/readabundance_fastbit/data/tara

if [[ ! -e $OUT_DIR ]]; then
  echo Making OUT_DIR \"$OUT_DIR\"
  mkdir -p $OUT_DIR
fi

echo Using IN_DIR \"$IN_DIR\"

PROG=$(basename $0 ".sh")
export FILE_LIST=$HOME/$PROG.in
INPUT_FILES_LIST=${1:-''}
if [[ ! -z $INPUT_FILES_LIST ]] && [ -e $INPUT_FILES_LIST ]; then
  cp $INPUT_FILES_LIST $FILE_LIST
else
  find $IN_DIR -type f | perl -MFile::Basename -ne 'print File::Basename::basename($_)' | sort | uniq > $FILE_LIST
fi

NUM_FILES=$(wc -l $FILE_LIST | cut -d ' ' -f 1)

echo Found \"$NUM_FILES\" distinct files 

if [ $NUM_FILES -lt 1 ]; then
  echo Nothing to do.
  exit 1
fi

qsub -J 1-$NUM_FILES:$STEP_SIZE -v FILE_LIST,STEP_SIZE,CWD,IN_DIR,OUT_DIR -N mk-csv -j oe -o out ./mk-csv.sh

echo The fox says bye.
