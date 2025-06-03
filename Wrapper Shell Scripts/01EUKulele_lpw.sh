#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J EUKulele
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=120GB

#Database setup and formatting
#Database creation, alignment, and taxonomic estimation
#Assessment of the BUSCO completeness of subsets of contigs at each taxonomic level
#Assessment of taxonomic classification using only BUSCO-identified core eukaryotic genes

# 开始计时
start_time=$(date +%s)

source activate EUKulele
#########################################################################
#真核基因组物种注释；使用PhyloDB、EukProt、MMETSP数据库；还集成了BUSCO的模块
#########################################################################


mags=/home/lipengwei2024phd/03GW_eu/00eu_36_bins
eukuleledir=/home/lipengwei2024phd/03GW_eu/01EUKulele

cd /home/lipengwei2024phd/03GW_eu/01EUKulele
echo "当前目录为：$(pwd)"

#该软件的物种注释基于蛋白基因比对，软件强烈建议输入每个MAGs的预测蛋白文件。
EUKulele --mets_or_mags mags --sample_dir ${mags}

# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0


#try
EUKulele --mets_or_mags mags --sample_dir /home/lipengwei2024phd/03GW_eu/04EUKulele/eMAG_protein/ --no_busco 1


