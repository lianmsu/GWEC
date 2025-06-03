#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J df
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=5
#SBATCH --mem=50GB

# 开始计时
start_time=$(date +%s)

source activate defensefinder
#########################################################################
# File Name: GO_DefenseFinder.sh
# Date: 17/4/2024
#########################################################################

euk_proteins=/home/lipengwei2024phd/03GW_eu/02EukCC/refine_workdir/faa
eukdefense=/home/lipengwei2024phd/03GW_eu/20defensefinder
output_dir="${eukdefense}/defense"

echo "当前目录为：$(pwd)"
cd /home/lipengwei2024phd/03GW_eu/20defensefinder
cat ${euk_proteins}/* > euk_all_36protein.faa

defense-finder run euk_all_36protein.faa --preserve-raw -o "$output_dir"

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0
