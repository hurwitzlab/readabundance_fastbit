#!/bin/bash

set -u

export CWD=$PWD
export STEP_SIZE=50
export IN_DIR=/rsgrps/bhurwitz/kyclark/pov/data/fastbit/csv
export OUT_DIR=/rsgrps/bhurwitz/kyclark/pov/data/fastbit/fb

if [[ ! -e $OUT_DIR ]]; then
  echo Making OUT_DIR \"$OUT_DIR\"
  mkdir -p $OUT_DIR
fi

PROG=$(basename $0 ".sh")
export FILES_LIST=$HOME/$PROG.in

INPUT_FILES_LIST=${1:-''}
if [[ ! -z $INPUT_FILES_LIST ]] && [ -e $INPUT_FILES_LIST ]; then
  echo Taking from \"$INPUT_FILES_LIST\"
  cp $INPUT_FILES_LIST $FILES_LIST
else
  echo Using IN_DIR \"$IN_DIR\"
  find $IN_DIR -type f > $FILES_LIST
fi

NUM_FILES=$(wc -l $FILES_LIST | cut -d ' ' -f 1)

echo Found \"$NUM_FILES\" distinct files 

if [ $NUM_FILES -lt 1 ]; then
  echo Nothing to do.
  exit 1
fi

qsub -I -v FILES_LIST,STEP_SIZE,CWD,IN_DIR,OUT_DIR -N mk-fb -j oe -o out $CWD/workers/mk-fastbit.sh

echo The fox says bye.
