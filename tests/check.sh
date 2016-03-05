#!/bin/sh

echo "Running tests"

for foo in tests/*.in
do
   echo " - $foo"
	$@ $foo -t 0 -n 14 -r > tests/out
   cmp $foo.out tests/out || exit 1
done

rm tests/out
