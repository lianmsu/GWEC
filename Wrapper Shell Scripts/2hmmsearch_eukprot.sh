#!/bin/sh
#SBATCH -o job.%j.%N.out
#SBATCH --partition=cpu
#SBATCH -J HMMeukprot
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --mem=100GB
#SBATCH --mail-type=end

source activate defensefinder2.0.0

for special_gene in Hachiman__HamB Zorya_TypeI__ZorD Brig1__ADP_ribosyl CBASS__AG_E1_ThiF Mokosh_type_I__MkoA_B Wadjet__JetC_III

do

target_path=/home/lipengwei2024phd/03GW_eu/40eu_contig/53secondaryHMM_eukprot
cd ${target_path}
mkdir -p ${special_gene}
cd ${special_gene}

HMM_file=/home/lipengwei2024phd/03GW_eu/40eu_contig/52eucontiggene_secondaryHMM/${special_gene}/${special_gene}_GA.hmm

cp ${HMM_file} ./

euproteins_path=/home/lipengwei2024phd/database/EukProt/all993eukaryotic_species_proteins.faa

hmmsearch --tblout eukprot_${special_gene}_GA_homologs.txt --cut_ga ${special_gene}_GA.hmm ${euproteins_path}

#hmmsearch --domtblout SngC.txt -E 1e-3 /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/1DefenseFinder_HMM/Shango__SngC.hmm /home/lipengwei2024phd/03GW_eu/01metaeuk/eMAG.fas

sed "s/ \+/\t/g" eukprot_${special_gene}_GA_homologs.txt | sed "/^#/d" | awk -F"\t" '$8 < 1e-5'| awk 'BEGIN {print "target name\taccession\tquery name\taccession\tE-value\tscore\tbias\tE-value\tscore\tbias\texp\treg\tclu\tov\tenv\tdom\trep\tinc\tdescription of target"} {print}' > eukprot_${special_gene}_GA_homologs2.txt

#按照E-value增序排列
#cd /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/2hmmsearch
(head -n 1 eukprot_${special_gene}_GA_homologs2.txt && tail -n +2 eukprot_${special_gene}_GA_homologs2.txt | sort -k8,8g) > eukprot_${special_gene}_GA_homologs2_eval_increase.txt
#提取faa序列
#cd /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/2hmmsearch
#awk -F"\t" 'NR>1 && NR<=201 {print $1}' SngC_eu_eval_increase.txt | seqkit grep -f - /home/lipengwei2024phd/03GW_eu/01metaeuk/eMAG.fas > eMAG_SngC_200.faa
#为提取的序列添加eukaryotes
#cd /home/lipengwei2024phd/03GW_eu/70selected_DS_phylogenetic/2hmmsearch
#echo -e "seqname\tsource" > eMAG_SngC_200.faa.id
#awk -F"\t" 'NR>1 && NR<=201 {print $1 "\teukaryotes"}' SngC_eu_eval_increase.txt >> eMAG_SngC_200.faa.id

done