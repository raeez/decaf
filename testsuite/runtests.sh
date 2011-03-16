#!/bin/bash

mkdir temp

while read dir
do
    ls $dir/*.dcf > temp/l
    while read line 
    do
	../src/test $line > temp/a.s
	ref=`echo $line | sed s/dcf/out/`
	echo $line $ref
        gccout=`gcc -o temp/a temp/a.s`
	temp/a > temp/o
	cmp=`fc $ref temp/o`
	echo $cmp
	err=`echo $cmp | wc -l`
	if [ $err -ne 0 ]; then
	    echo -----$line------
	    echo expected: 
	    cat $ref
	    echo got:
	    cat temp/o
	fi
    done < temp/l
done < list

