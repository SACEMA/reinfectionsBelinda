#!/bin/bash

#PBS -P CBBI1106
#PBS -N l2_third_infections
#PBS -l select=1:ncpus=24
#PBS -o /mnt/lustre/users/blombard/reinfectionsBelinda/oe_files/l2_third_infections.out
#PBS -e /mnt/lustre/users/blombard/reinfectionsBelinda/oe_files/l2_third_infections.err
#PBS -l walltime=1:00:00
#PBS -J 1-11:11
#PBS -q smp
#PBS -M belindalombard@sun.ac.za
#PBS -m be

sleep 4

#module add chpc/BIOMODULES R/4.1.0

# Add R module (includes appropriate openMPI and gcc modules)
module add chpc/R/3.5.1-gcc7.2.0

# make sure we're in the correct working directory.
cd /mnt/lustre/users/blombard/reinfectionsBelinda/

make l2_sbv

STEP=11
START=$PBS_ARRAY_INDEX
END=$(( $START+$STEP-1 ))

for i in $(eval echo "{$START..$END}")
do
	(echo "process $i started" && Rscript sbv/third_infections/third_infections_l2.R $i && echo "process $i finished")& 
done

sleep 0.1 # For sequential output
echo "Waiting for processes to finish" 
wait $(jobs -p)0
echo "All processes finished"
