#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J eukcc
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=120GB

#########################################################################
# eukcc真核基因组质量评估；其原理是基于动态变化的单拷贝标记基因集（SCMGs），包括基础真核生物、真菌、原生动物及植物的SCMGs。
# SCMGs的原理在于基于特异物种谱系通用的单个拷贝的基因，所以可以比较预测的基因数量来评估完整性，而额外的SCMGs则是污染序列。
# EukCC将在8个线程上运行。你可以将核苷酸fastas或蛋白质组传递到EukCC。它会自动检测是否需要预测蛋白质。
# 第一步，使用Genemark-ES预测输入基因组中的蛋白质
# 第二步，使用hmmer对预测的蛋白质与所选SCMG集中的HMM比对。
# 最后，Finally, EukCC reports a lowest common ancestor lineage of the input genome, based on the species within the marker set.
# EukCC文章主要比较的是BUSCO；有一定优势（得到的完整度更高，污染度估计更保守）
# 快速下载工具：https://www.freedownloadmanager.org/zh/
#########################################################################

# 开始计时
start_time=$(date +%s)
#环境、路径设置
source activate eukcc

mags_path=/home/lipengwei2024phd/03GW_eu/00eu_36_bins
eukcc_dir=/home/lipengwei2024phd/03GW_eu/02EukCC
eukccdb_path=/home/lipengwei2024phd/database/eukccdb/eukcc2_db_ver_1.1

cd ${eukcc_dir}
echo "当前目录为：$(pwd)"

#folder模式预测
eukcc folder --out ${eukcc_dir} --threads 8 ${mags_path} --db ${eukccdb_path}

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0


