BASE_DIR=/home/u20/kyclark/work/readabundance_fastbit/data/16s
IN_DIR=$BASE_DIR/csv
OUT_DIR=$BASE_DIR/fastbit

FILES_LIST=$(mktemp)
find $IN_DIR -name \*.csv > $FILES_LIST
NUM_FILES=$(wc -l $FILES_LIST | cut -d ' ' -f 1)

echo Found \"$NUM_FILES\" files in \"$IN_DIR\"

if [[ ! -d $OUT_DIR ]]; then
    mkdir -p $OUT_DIR
fi

i=0
while read FILE; do
    let i++
    BASENAME=$(basename $FILE)
    printf "%5d: %s\n" $i $BASENAME

    COLS=$(head -1 $FILE | perl -F',' -nae 'print join ",", map { "f$_:int"} 1..$#F')

    if [ -z "$COLS" ]; then
        echo Something went wrong trying to get columns
        break
    fi

    DIR=$OUT_DIR/$BASENAME 
    if [ -d $DIR ]; then
        rm -rf $DIR
    fi

    ardea -d $DIR -m \"rid:int,$COLS\" -t $FILE
done < $FILES_LIST
