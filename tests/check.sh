#!/bin/sh

echo "Running tests"

for foo in tests/*.in
do
   FLAGS=$(cat $foo | grep "flags: " | sed -e 's/.*flags: //')
   echo " - $@ $FLAGS $foo"
	$@ $FLAGS $foo > tests/out
   diff $foo.out tests/out || exit 1
done

rm tests/out
