#!/bin/bash

# ./run.sh dataset_name number_bands number_per_bands nrows ncols

bin/lsh $1 $2 $3
scripts/sortcommand LSH/$1.lsh
bin/candidates $1 $4
scripts/sortcommand Candidates/$1.cand
bin/regions $1 $4 $5
scripts/sortcommand Biclusters/$1.region
scripts/sortcommand Biclusters/$1.expanded
bin/inclose $1 $4 $5
scripts/sortcommand Biclusters/$1.biclusters
bin/stats $1
