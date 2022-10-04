#!/bin/bash

#SBATCH --job-name=models # the name for the cluster scheduler
#SBATCH --time=35:30:00 # Maximum allowed runtime per iteration
#SBATCH --mem-per-cpu=70G
#SBATCH --array=1-755 # the number of iterations
#SBATCH --output=output_messages/model_%A_%a.out # the name of the output files
#SBATCH --mail-type=ALL
#SBATCH --mail-user=david.hemprich-bennett@zoo.ox.ac.uk

module purge
module load Anaconda3//2020.11

source activate $DATA/tiago_env

Rscript scripts/8_Final_models.R ${SLURM_ARRAY_TASK_ID} 
