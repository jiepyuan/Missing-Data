#!/bin/bash
#SBATCH -N 1
#SBATCH -t 1-00:00:00
#SBATCH -p standard
#SBATCH -A your_allocation_account

module load goolf R
Rscript Simulation_Script_n50.R
