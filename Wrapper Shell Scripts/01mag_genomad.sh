#!/bin/sh
#SBATCH -o job.genomad333.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J genomad
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --mem=100GB

# 开始计时
start_time=$(date +%s)


source activate genomad
genomad_db=/home/lipengwei2024phd/database/genomad_db
#########################################################################


cd /home/lipengwei2024phd/03GW_eu/10genomad


mags=/home/lipengwei2024phd/03GW_eu/10genomad/eMAGs_contig.fa
mag_genomad=/home/lipengwei2024phd/03GW_eu/10genomad

#mkdir -p "${magdefense}/${i}/${j##*/}"

fasta_filename=$(basename "$mags")
echo "fasta 文件：${fasta_filename}"
output_dir="${mag_genomad}/res"
echo "输出目录：${output_dir}"
echo "Running genomad"
genomad end-to-end --cleanup ${mags} ${output_dir} ${genomad_db}



# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "脚本运行时间：$runtime 秒"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0



