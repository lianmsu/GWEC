#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J 5kbdeconta_derep
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --mem=500GB
#SBATCH --mail-type=end

source activate eggnog
#toolpath=/lustre/home/liutang_faculty/21rhodopsin_CQ/03software/cd-hit-v4.6.8-2017-1208
outpath=/home/lipengwei2024phd/03GW_eu/40eu_contig/31eu_5kbcontig_decontami_derep
cd ${outpath}
#cd-hit-est -i /home/lipengwei2024phd/03GW/03mag_orfs/all_27578_orf/all_nucleotide_seq.fa -o ${outpath}/nucl27578_cdhit.fna -c 0.95 -aS 0.9 -M 0 -d 0 -T 12 -n 5

#cd-hit -i /home/lipengwei2024phd/03GW_eu/40eu_contig/31eu_5kbcontig_decontami_derep/5kbcontig_decontami.fna -o ${outpath}/5kbcontig_decontami_derep.fna -c 0.95 -aS 0.85 -M 0 -d 0 -T 48 -n 5

#参考https://mp.weixin.qq.com/s/gE7pq0_Pepp8MzTilgvYvQ
mmseqs easy-cluster /home/lipengwei2024phd/03GW_eu/40eu_contig/31eu_5kbcontig_decontami_derep/5kbcontig_decontami.fna mmseqs tmp -c 0.85 --cov-mode 1 --min-seq-id 0.95 --cluster-mode 2 --threads 48


#transeq -sequence uniqGeneSet.ffn -table 11 -trim -outseq uniqGeneSet.faa


