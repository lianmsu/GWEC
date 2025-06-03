#!/bin/sh
#SBATCH -o job.tree.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J MkoAiq
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=120GB


source activate iqtree

special_gene=Mokosh_type_I__MkoA_B
cd /home/lipengwei2024phd/03GW_eu/40eu_contig/51eucontiggene_DG_phylogenetic/${special_gene}

#cat /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/0prokaryotic_DG/bac_MAG_SngC_200HMM_notimmunity.faa /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/0prokaryotic_DG/bac_MAG_SngC_DG.faa /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/2hmmsearch/eMAG_SngC_200.faa > SngC_bac_eu_600.faa


#1、先用muscle把protein序列对齐形成多序列比对（MSA）
#muscle -align SngC_bac_eu.faa -output SngC_bac_eu_aln.afa -threads 24

mafft --auto --thread 24 "${special_gene}_all.faa" > ${special_gene}_mafft.afa

#2、然后用trimal修剪
clipkit ${special_gene}_mafft.afa -m kpic-gappy -g 0.9
#-gt 0.5代表删去有50%以上gap的位点。-gt 0.5这个是对应这篇文章方法中的参数 https://www.nature.com/articles/s41467-023-38400-0

#3、再用fasttree对修建后的MSA进行建树
iqtree -s ${special_gene}_mafft.afa.clipkit -m VT+F+I+G4 -bb 1000 -nt AUTO

echo "${special_gene} tree finised!"

