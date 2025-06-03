######################################
setwd("D:/pku/01groundwater/02eukaryotes/02busco")
# Load the required libraries
library(ggplot2)
library("grid")

#读取
my_data <- read.csv("batch_summary.txt",sep = '\t')

my_data$Input_file <- gsub("_renamed.fa","",my_data$Input_file)

library(tidyr)
my_data_long <- my_data %>%
  pivot_longer(-c(Input_file,Dataset,Complete,n_markers,Scaffold.N50,Contigs.N50,Percent.gaps,Number.of.scaffolds), 
               names_to = "variable", values_to = "value")

# 将variable列转换为因子类型，并指定顺序
my_data_long$variable <- factor(my_data_long$variable, levels = c("Missing","Fragmented","Duplicated","Single"))

# 设置 my_data_long$Input_file 的因子顺序
my_data_long$Input_file <- factor(my_data_long$Input_file, levels = rev(clustered_row_names))

p2 <- 
ggplot(data = my_data_long, aes(x = Input_file, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  theme_gray(base_size = 8) +
  scale_y_continuous(labels = c("0","20","40","60","80","100"), breaks = c(0,20,40,60,80,100)) +
  scale_fill_manual(values = c("Single"="#ffa69e","Duplicated"="#faf3dd",
                               "Fragmented"="#b8f2e6","Missing"="#aed9e0"
                               ),
             ) +
#  ggtitle(my_title) +
  xlab("") +
  ylab("\n%BUSCOs") +
  theme_minimal(base_size = 10) +  # 使用简洁的主题
  theme(
    plot.title = element_text(hjust = 0.5, colour = "black", size = rel(1.5) * 1, face = "plain"),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) +
  guides(fill = guide_legend(override.aes = list(colour = NULL), nrow = 1, byrow = TRUE))
p2
###########################################################################
setwd("D:/pku/01groundwater/02eukaryotes/02eukcc")
#读取
my_data <- readxl::read_excel("eukcc.xlsx")
my_data <- my_data[,1:3]
#my_data <- read.csv("eukcc.csv",sep = '\t')

my_data$bin <- gsub("_renamed.fa","",my_data$bin)

library(tidyr)
my_data_long <- my_data %>%
  pivot_longer(-c(bin), 
               names_to = "variable", values_to = "value")

# 将variable列转换为因子类型，并指定顺序
my_data_long$variable <- factor(my_data_long$variable, levels = c("contamination","completeness"))

# 设置 my_data_long$Input_file 的因子顺序
my_data_long$bin <- factor(my_data_long$bin, levels = rev(clustered_row_names))

p3 <- 
ggplot(data = my_data_long, aes(x = bin, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  theme_gray(base_size = 8) +
  scale_y_continuous(labels = c("0","20","40","60","80","100"), breaks = c(0,20,40,60,80,100)) +
  scale_fill_manual(values = c("completeness"="#ffa69e",
                               "contamination"="#aed9e0"
  ),
  ) +
  #ggtitle("EUKCC Results") +
  xlab("") +
  ylab("\n%EUKCC") +
  theme_minimal(base_size = 10) +  # 使用简洁的主题
  theme(
    plot.title = element_text(hjust = 0.5, colour = "black", size = rel(1.5) * 1, face = "plain"),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) +
  guides(fill = guide_legend(override.aes = list(colour = NULL), nrow = 1, byrow = TRUE))
p3


#p1来自"D:\pku\01groundwater\03eu_pre\15euMAG_ANI\1ANI.R"

library(cowplot)
g1 <- plot_grid(p2,p3,labels = letters[1:2], nrow = 1,rel_heights = c(1,1))
g1

setwd("D:/pku/01groundwater/02eukaryotes/02busco")
ggsave(plot = g1,'1_busco_eukcc_quality.pdf',width = 7.8,height = 3.63,units = 'in')




