#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J metaeuk
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=120GB

# 开始计时
start_time=$(date +%s)

source activate busco
#########################################################################
#真核基因组质量评估；利用odb数据库（同源基因数据库）；默认metaeuk预测基因+HMM比对
#########################################################################
#busco --download eukaryota_odb10


mags=/home/lipengwei2024phd/03GW_eu/00eu_36_bins
buscodir=/home/lipengwei2024phd/03GW_eu/02BUSCO

cd /home/lipengwei2024phd/03GW_eu/02BUSCO
echo "当前目录为：$(pwd)"

busco -v


busco --in ${mags} --lineage_dataset /home/lipengwei2024phd/03GW_eu/02BUSCO/busco_downloads/lineages/eukaryota_odb10 --out res --mode genome --cpu 12 --metaeuk --offline

#计算节点有网络时：
#busco --in ${mags} --lineage_dataset eukaryota_odb10 --out res --mode genome --cpu 12 --metaeuk --download_path /home/lipengwei2024phd/03GW_eu/02BUSCO/busco_downloads



# 结束计时
end_time=$(date +%s)
# 计算运行时间
runtime=$((end_time - start_time))
echo "running time：$runtime s"
#输出完成消息
tput setaf 2; echo "Done!"; tput sgr0





#mkdir my_summaries
#find /home/lipengwei2024phd/03GW_eu/02BUSCO/res -type f -name 'short_summary.specific.eukaryota_odb10.*.txt' -exec cp -p {} /home/lipengwei2024phd/03GW_eu/02BUSCO/my_summaries/ \;
#generate_plot.py -wd my_summaries
