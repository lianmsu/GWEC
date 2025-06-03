#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J cdhit_eucontig
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --mem=500GB
#SBATCH --mail-type=end

source activate cd-hit
#toolpath=/lustre/home/liutang_faculty/21rhodopsin_CQ/03software/cd-hit-v4.6.8-2017-1208
outpath=/home/lipengwei2024phd/03GW_eu/40eu_contig/20eu_contig_derep

#cd-hit-est -i /home/lipengwei2024phd/03GW/03mag_orfs/all_27578_orf/all_nucleotide_seq.fa -o ${outpath}/nucl27578_cdhit.fna -c 0.95 -aS 0.9 -M 0 -d 0 -T 12 -n 5

cd-hit -i /home/lipengwei2024phd/03GW_eu/40eu_contig/all_eu_contig.fasta -o ${outpath}/all_eu_contig_cdhit.fna -c 0.95 -M 0 -d 0 -T 48 -n 5
#transeq -sequence uniqGeneSet.ffn -table 11 -trim -outseq uniqGeneSet.faa


