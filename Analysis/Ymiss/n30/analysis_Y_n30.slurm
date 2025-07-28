#!/bin/bash
#SBATCH -n 1
#SBATCH -t 5-00:00:00
#SBATCH --array=1-144
#SBATCH --partition=standard
#SBATCH -A your_allocation_account

module load goolf R
Rscript R${SLURM_ARRAY_TASK_ID}.R
