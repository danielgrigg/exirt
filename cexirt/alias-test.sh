#!/bin/sh

echo "$1 tests"
#lein uberjar

for i in 1 2 4 16 64 256
do
  echo "${i} sample test"
  #lein run 512 512 40 $i 
  OUT_FILE=~/Documents/exirt/test_images/alias-$1-wx-s${i}.exr
  if [ ! -e $OUT_FILE ]
  then
    java -server -jar ./target/cexirt-1.0.0-SNAPSHOT-standalone.jar 512 512 40 $i
    mv /tmp/basic_exr.exr $OUT_FILE
    say "render ${i} done"
  else 
    echo "${OUT_FILE} exists."
  fi
done

