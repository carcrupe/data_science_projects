#!/bin/bash
DELIMITER=$1
FILE=$2
echo 'Delimiter:'$DELIMITER 'File:'$FILE


NUM_COLUMNS=$(head -1 $FILE | tr "$DELIMITER" "\n" | wc -l)
paste <(seq $NUM_COLUMNS) <(head -1 $FILE | tr "$DELIMITER" "\n")
