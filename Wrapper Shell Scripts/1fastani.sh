#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J fani
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem=250GB

source activate drep

# 开始计时
start_time=$(date +%s)

input_filelist=/home/lipengwei2024phd/03GW_eu/000fastani/fa_files_paths.txt

output_path=/home/lipengwei2024phd/03GW_eu/000fastani
cd ${output_path}

fastANI --ql ${input_filelist} --rl ${input_filelist} -o fastani_out2.txt -t ${SLURM_NTASKS} --matrix

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0
