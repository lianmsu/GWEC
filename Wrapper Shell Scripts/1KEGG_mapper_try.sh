#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J kofam_eMAG
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem=250GB


source activate kofamscan

input_proteins=/home/lipengwei2024phd/03GW_eu/01metaeuk/eMAG.fas
output_path=/home/lipengwei2024phd/03GW_eu/23eMAG_kegg
database_path=/home/lipengwei2024phd/database/kofam
cd ${output_path}

# 开始计时
start_time=$(date +%s)
#echo "当前目录为：$(pwd)"

exec_annotation -f mapper -o ${output_path}/36euMAG_uniref90_protein.txt --profile ${database_path}/profiles/  -k ${database_path}/ko_list --cpu $SLURM_NTASKS ${input_proteins}

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0
