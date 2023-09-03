#!/bin/bash

#PBS -P CBBI1106
#PBS -N array_job
#PBS -l select=1:ncpus=24
#PBS -o /mnt/lustre/users/blombard/reinfectionsBelinda/oe_files/array_job_m1.out
#PBS -e /mnt/lustre/users/blombard/reinfectionsBelinda/oe_files/array_job_m1.err
#PBS -l walltime=1:00:00
#PBS -J 1-21:21
#PBS -q smp
#PBS -M belindalombard@sun.ac.za
#PBS -m be

sleep 4

#module add chpc/BIOMODULES R/4.1.0

# Add R module (includes appropriate openMPI and gcc modules)
module add chpc/R/3.5.1-gcc7.2.0

# make sure we're in the correct working directory.
cd /mnt/lustre/users/blombard/reinfectionsBelinda/

make sbv

STEP=21
START=$PBS_ARRAY_INDEX
END=$(( $START+$STEP-1 ))

for i in $(eval echo "{$START..$END}")
do
	(echo "process $i started" && Rscript sbv/method_1_analysis/method_1_arrayjob.R $i && echo "process $i finished")& 
done

sleep 0.1 # For sequential output
echo "Waiting for processes to finish" 
wait $(jobs -p)
echo "All processes finished"

