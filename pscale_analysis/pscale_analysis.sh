#!/bin/bash

#i=1
#echo $i > pscale.txt
#sleep 1
#make pscale_analysis_1

rm output/$1_pscale_analysis.RDS

for i in $(seq 1 0.1 3) 
do
	echo $i > pscale.txt
	sleep 1
	make output/$1_pscale_analysis.RDS
done
