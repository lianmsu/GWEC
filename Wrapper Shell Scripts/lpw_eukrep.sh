#!/bin/sh
#SBATCH -o job.coverm.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J eukrep
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --mem=500GB

start_time=$(date +%s)
#module load samtools 在coverm环境中有
#module load Bowtie2/2.4.2-GCC-10.2.0

source activate eukrep-env

res=/home/lipengwei2024phd/03GW_eu/40eu_contig/1eukrep
contigs=/home/groundwaterpublic/share/contigs

cd ${res}
while IFS= read -r i
do

echo "${i} mapping"
start_time=$(date +%s)

mkdir -p ${res}/${i}
#
EukRep -i ${contigs}/${i}/final_assembly.fasta -o ${res}/${i}/${i}_euk.fasta -ff --min 1000


done < /home/lipengwei2024phd/03GW_eu/40eu_contig/all_sample_name.txt
