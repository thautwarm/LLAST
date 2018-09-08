#!/bin/sh
for a in ../ir-snippets/*.ll
do
	llc-6.0 $a -o $a.s;
	gcc -C $a.s -o $a.out;
	./$a.out;
	e=$?;
	a=$(echo $a| cut -f3 -d/ | cut -f1 -d. ) ;
	printf "$e \treturned by $a\n";
done
