#!/bin/bash

#SBATCH --job-name=pseudoabsence # the name for the cluster scheduler
#SBATCH --time=12:30:00 # Maximum allowed runtime per iteration
#SBATCH --mem-per-cpu=70G
#SBATCH --array=1-755 # the number of iterations
#SBATCH --output=output_messages/pseudoabsence%A_%a.out # the name of the output files
#SBATCH --mail-type=ALL
#SBATCH --mail-user=david.hemprich-bennett@zoo.ox.ac.uk

module purge
module load Anaconda3//2020.11

source activate $DATA/tiago_env

Rscript scripts/7_pseudoabsence_biomod2.R ${SLURM_ARRAY_TASK_ID} 
