head -1 optd_aircraft.csv | tr "^" "\n" | wc -l
paste <(seq 8) <(head -1 optd_aircraft.csv | tr "^" "\n")
