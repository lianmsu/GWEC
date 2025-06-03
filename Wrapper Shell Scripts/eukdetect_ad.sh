#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J Eudetectad
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem=250GB


source activate eukdetect
work_dir=/home/lipengwei2024phd/03GW_eu/31eukdetect

eukdetect --mode runall --configfile ${work_dir}/my_configfile_ad.yml --cores $SLURM_NTASKS

echo "ad finished"
