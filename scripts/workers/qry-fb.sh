DATA_DIR=/rsgrps/bhurwitz/kyclark/george-16s/data/fastbit/fb
OUT_DIR=/rsgrps/bhurwitz/kyclark/george-16s/data/fastbit/qry

if [[ ! -d $OUT_DIR ]]; then
  mkdir -p $OUT_DIR
fi

DIRS=$(mktemp)
find $DATA_DIR -mindepth 1 -maxdepth 1 -type d > $DIRS

i=0
while read DIR; do
  let i++
  BASENAME=$(basename $DIR)
  OUT=$OUT_DIR/$BASENAME

  printf "%5d: %s\n" $i $BASENAME

  ibis -d $DIR -q 'select rid where pct<=20' -o $OUT
done < $DIRS

echo Done.
