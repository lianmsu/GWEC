#!/bin/sh
#SBATCH -o job.index.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J index
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --mem=100GB

start_time=$(date +%s)

source activate bowtie2
bins=/home/lipengwei2024phd/03GW_eu/00eu_36_bins

cat ${bins}/*.fa > ./merge.fa

####Step1:对contig建立index索引文件
echo "index build start"

mkdir index
bowtie2-build merge.fa index/all

end_time=$(date +%s)
cost_time=$[ $end_time-$start_time ]
echo "Total index build execution time is $(($cost_time/60))min " 
