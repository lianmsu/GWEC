#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J 5kb_dd_euk
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --mem=500GB

source activate metaeuk

input_contigs=/home/lipengwei2024phd/03GW_eu/40eu_contig/31eu_5kbcontig_decontami_derep/mmseqs_rep_seq.fasta
database_path=/home/lipengwei2024phd/database/metaeuk_database/UniRef90/uniref90
output_path=/home/lipengwei2024phd/03GW_eu/40eu_contig/32eu_5kb_ddcontig_metaeuk
cd ${output_path}

# 开始计时
start_time=$(date +%s)
#echo "当前目录为：$(pwd)"
# fasta_filename=$(basename "$mags")
# echo "fasta 文件：${fasta_filename}"
# output_dir="${magdefense}/${fasta_filename%.*}"
# echo "输出目录：${output_dir}"
# echo "Running DefenseFinder"
#metaeuk easy-predict contigsFasta/contigsDB proteinsFasta/referenceDB predsResults tempFolder

#cat ${input_bin}/*fa >${output_path}/eMAGs_contig.fa
# 预测基因
metaeuk easy-predict ${input_contigs} ${database_path} eu_5kb_ddcontig tmp --threads ${SLURM_NTASKS}
# 分配分类学
metaeuk taxtocontig ${output_path}/tmp/latest/contigs ${output_path}/eu_5kb_ddcontig.fas ${output_path}/eu_5kb_ddcontig.headersMap.tsv ${database_path} eu_5kb_ddcontig_tax tmp_tax --majority 0.5 --tax-lineage 1 --lca-mode 2 --threads ${SLURM_NTASKS}

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0
