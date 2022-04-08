#!/bin/bash

#SBATCH --job-name=thinning # the name for the cluster scheduler
#SBATCH --time=01:30:00 # Maximum allowed runtime per iteration
#SBATCH --array=1-722 # the number of iterations
#SBATCH --output=output_messages/thinning%A_%a.out # the name of the output files
#SBATCH --mail-type=ALL
#SBATCH --mail-user=david.hemprich-bennett@zoo.ox.ac.uk

module purge
module load Anaconda3//2020.11

source activate $DATA/tiago_env

Rscript scripts/3_thinning.R ${SLURM_ARRAY_TASK_ID} 
