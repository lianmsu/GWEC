#!/bin/sh
#SBATCH -o job.coverm.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J coverm
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --mem=500GB

start_time=$(date +%s)
#module load samtools 在coverm环境中有
#module load Bowtie2/2.4.2-GCC-10.2.0

source activate coverm


reads=/home/lipengwei2024phd/03GW/00clean_reads/GW_metagenome
bins=/home/lipengwei2024phd/03GW_eu/00eu_36_bins
res=/home/lipengwei2024phd/03GW_eu/30coverm

cd ${res}
mkdir -p sorted_bam2 #存放排序后的bam
#for i in A814 B814 C814 L1-814 L2-814 L3-814 L4-814 L5-814

cd ${res}
while IFS= read -r i
do

echo "${i} mapping"
start_time=$(date +%s)

mkdir ${res}/${i}
#bowtie2进行mapping
bowtie2 -x ${res}/index/all -1 ${reads}/${i}/$i\_clean_1.fastq.gz -2 ${reads}/${i}/$i\_clean_2.fastq.gz -S ${res}/${i}/${i}.sam --threads $SLURM_NTASKS
#sam转化为bam并排序
samtools view -@ $SLURM_NTASKS -b -S ${res}/${i}/${i}.sam > ${res}/${i}/${i}.bam
samtools sort -@ $SLURM_NTASKS ${res}/${i}/${i}.bam > ${res}/sorted_bam2/${i}.sorted.bam

rm -rf ${res}/${i}
end_time=$(date +%s)
cost_time=$[ $end_time-$start_time ]

coverm genome \
-m rpkm \
--bam-files sorted_bam2/${i}.sorted.bam \
--genome-fasta-directory genomes_fna  \
--min-read-percent-identity 95 \
--min-read-aligned-percent 90 \
--output-file eukMAGs_${i}_abundance_rpkm.tsv \
--contig-end-exclusion 0 \
--no-zeros \
-t $SLURM_NTASKS

#echo "${i} $SLURM_JOB_NODELIST   $SLURM_NTASKS    execution time is $(($cost_time/60))min " >> Running_time_${SLURM_JOBID}.log

done < /home/lipengwei2024phd/03GW_eu/30coverm/all_sample_name.txt

#cd ${res}
#mkdir genomes_fna
#cd genomes_fna
#ln -s ${bins}/* ./
#rename fa fna *fa
#cd ..

coverm_end_time=$(date +%s)
coverm_cost_time=$[ $coverm_end_time-$coverm_start_time ]
echo "abundance calculate time is $(($coverm_cost_time/60))min " >> Running_time_${SLURM_JOBID}.log

