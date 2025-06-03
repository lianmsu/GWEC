
gene="Mokosh_type_I__MkoA_B"
secondHMM_res=/home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/${gene}
eukprot_database_path=/home/lipengwei2024phd/database/EukProt/all993eukaryotic_species_proteins.faa

#提取真核对应的蛋白序列100个
mkdir -p ${secondHMM_res}
cd ${secondHMM_res}
secondHMM_res_file=${secondHMM_res}/eukprot_${gene}_GA_homologs2_eval_increase.txt
awk 'NR>1 && NR<=101 {print $1}' ${secondHMM_res_file} | seqkit grep -f - ${eukprot_database_path} > selected_eukprot_${gene}.faa
#为提取的序列添加第二列：eukprot_immunity
echo -e "seqname\tsource" > selected_eukprot_${gene}.faa.id
awk -F"\t" 'NR>1 && NR<=101 {print $1 "\teukprot_immunity"}' ${secondHMM_res_file} >> selected_eukprot_${gene}.faa.id

#细菌的
#变量准备
prokaryotes=/home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/0prokaryotic_DG
bacteria_protein_file=/home/lipengwei2024phd/03GW/03mag_orfs/all_27578_orf/all_protein_seq.fa
gene_phylo_path=/home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/${gene}
#DG结果根据E-value从小到大排序
cd ${gene_phylo_path}
(head -n 1 ${prokaryotes}/all_merged_defense_finder_genes.tsv && tail -n +2 ${prokaryotes}/all_merged_defense_finder_genes.tsv | grep "${gene}" | sort -k15,15g) > bac_DG_${gene}.tsv
#提取bac_DG_${special_gene}.tsv前200个蛋白序列
awk -F"\t" 'NR>1 && NR<=101 {print $2}' bac_DG_${gene}.tsv | seqkit grep -f - ${bacteria_protein_file} > bac_MAG_${gene}_DG.faa
#为提取的序列添加第二列：bacteria_immunity
echo -e "seqname\tsource" > bac_MAG_${gene}_DG.faa.id
awk -F"\t" 'NR>1 && NR<=101 {print $2 "\tbacteria_immunity"}' bac_DG_${gene}.tsv >> bac_MAG_${gene}_DG.faa.id

#古菌的
#变量准备
archaea_defense=/home/lipengwei2024phd/03GW_archaea/03defensefinder/archaea2269mag_defense2.0.0
archaea_protein_file=/home/lipengwei2024phd/03GW_archaea/03defensefinder/archaea2269mag_defense2.0.0/archaea2269mag.prt
gene_phylo_path=/home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/${gene}

#DG结果根据E-value从小到大排序
cd ${gene_phylo_path}
(head -n 1 ${archaea_defense}/archaea2269mag_defense_finder_genes.tsv && tail -n +2 ${archaea_defense}/archaea2269mag_defense_finder_genes.tsv | grep "${gene}" | sort -k15,15g) > ar_DG_${gene}.tsv

#提取ar_DG_${special_gene}.tsv前200个蛋白序列
awk -F"\t" 'NR>1 && NR<=101 {print $2}' ar_DG_${gene}.tsv | seqkit grep -f - ${archaea_protein_file} > ar_MAG_${gene}_DG.faa
#为提取的序列添加第二列：archaea_immunity
echo -e "seqname\tsource" > ar_MAG_${gene}_DG.faa.id
awk -F"\t" 'NR>1 && NR<=101 {print $2 "\tarchaea_immunity"}' ar_DG_${gene}.tsv >> ar_MAG_${gene}_DG.faa.id





#外类群手动整理

#合并上面得到的文件
#合并得到的faa文件和faa.id文件
cd ${gene_phylo_path}
cat selected_eukprot_${gene}.faa \
	bac_MAG_${gene}_DG.faa \
	ar_MAG_${gene}_DG.faa \
	outgroup_${gene}_bac.fa \
	outgroup_${gene}_euk.fa \
	> ${gene}_all.faa
#人类同源的基因手动添加！！！

#合并下面5个文件，注意标题行只保留一次。
head -n 1 selected_eukprot_${gene}.faa.id > ${gene}_merged_file.id
tail -n +2 selected_eukprot_${gene}.faa.id >> ${gene}_merged_file.id
tail -n +2 bac_MAG_${gene}_DG.faa.id >> ${gene}_merged_file.id
tail -n +2 outgroup_${gene}.id >> ${gene}_merged_file.id
tail -n +2 ar_MAG_${gene}_DG.faa.id >> ${gene}_merged_file.id

grep '>' /home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/Mokosh_type_I__MkoA_B/Mokosh_type_I__MkoA_B_all.faa | wc -l
cat /home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/Mokosh_type_I__MkoA_B/Mokosh_type_I__MkoA_B_merged_file.id | wc -l

############
source activate iqtree
#gene=Hachiman__HamB
cd /home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot/${gene}

mafft --auto --thread 24 "${gene}_all.faa" > ${gene}_mafft.afa

clipkit ${gene}_mafft.afa -m kpic-gappy -g 0.95

iqtree -s ${gene}_mafft.afa.clipkit -m LG+G4 -B 1000 -nt AUTO
