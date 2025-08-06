#!/bin/bash

rm -f ./result/rvwmo_5.txt
rm -f ./result/rvwmo_6.txt
rm -f ./result/sketch_5.txt
rm -f ./result/sketch_6.txt

echo "Running rvwmo_5.rkt..."
racket rvwmo_5.rkt > ./result/rvwmo_5.txt

echo "Running rvwmo_6.rkt..."
racket rvwmo_6.rkt > ./result/rvwmo_6.txt

echo "Running sketch_5.rkt..."
racket sketch_5.rkt > ./result/sketch_5.txt

echo "Running sketch_6.rkt..."
racket sketch_6.rkt > ./result/sketch_6.txt

echo "All scripts finished."
