library(ggplot2)
library(ggplot2)
library(ggbeeswarm)
library(matrixcalc) #triangular matrix
library(reshape2) ##melt works better than with tidyverse, I'm sorry
library(ggpubr)
## plot distances


## 
plot_distances <- function(csv_filename, already_triangular=TRUE) {
  table <- read.csv(csv_filename, row.names = 1)
  if(already_triangular==FALSE) table <- lower.triangle(as.matrix(table))
  print(max(table))
  # get back zeroes
  table[is.na(table)] <- 0
  table <- as.matrix(table)
  print(max(table))
  # melt
  table_long <- melt(table)
  table_long <- table_long[table_long$value > 0, ]
  table_long$first <- unlist(sapply(strsplit(x = as.character(table_long$Var1), split = "_"), FUN = function(x) {x[length(x)]}))
  table_long$second <- unlist(sapply(strsplit(x = as.character(table_long$Var2), split = "_"), FUN = function(x) {x[length(x)]}))
  # group = biologica species
  table_long$group <- paste(substr(table_long$first, 4,4), substr(table_long$second,4,4), sep = "\n")
  ## remove intraspecific differences
  table_long <- table_long[!(table_long$group %in% c("W\nW", "S\nS", "E\nE")), ]
  myplot <-   ggplot(table_long, aes(x=group, y=value)) +
    geom_jitter(size=0.5) + 
    #geom_boxplot() + 
#    geom_beeswarm() + 
    geom_hline(yintercept = 0.16, linetype = "dotted") + 
    expand_limits(y=c(0, 0.2)) + 
    scale_x_discrete(limits = rev) + 
    scale_y_continuous(n.breaks=6) + 
    xlab("") + ylab("") + 
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(hjust=0.5, size=14))
  return(myplot)
}


## patristic distances with patristic
patrdist <- plot_distances("mb_patristic/patristic_matrix.ed.csv", already_triangular = FALSE) + ggtitle("Patristic")

##### 
# patristic_table <- read.csv("mb_patristic/patristic_matrix.ed.csv", row.names = 1)
# patristic_table_triangular <- lower.triangle(as.matrix(patristic_table))
# max(patristic_table_triangular)
# 
# 
# patristic_long <- melt(patristic_table_triangular)
# patristic_long <- patristic_long[patristic_long$value > 0, ]
# patristic_long$first <- unlist(sapply(strsplit(x = as.character(patristic_long$Var1), split = "_"), FUN = function(x) {x[length(x)]}))
# patristic_long$second <- unlist(sapply(strsplit(x = as.character(patristic_long$Var2), split = "_"), FUN = function(x) {x[length(x)]}))
# 
# patristic_long$group <- paste(substr(patristic_long$first, 4,4), substr(patristic_long$second,4,4), sep = " vs. ")
# ## remove intraspecific differences
# patristic_long <- patristic_long[!(patristic_long$group %in% c("W vs. W", "S vs. S", "E vs. E")), ]
# 
# ggplot(patristic_long, aes(x=group, y=value)) +
#   geom_jitter() + 
#   geom_hline(yintercept = 0.16, linetype = "dotted") + ylab("patristic") +
#   expand_limits(y=c(0, 0.3)) + 
#   theme_bw()
###### 
## ml iqtree works strange
##### 
## ml
# disttable <- read.table("iqtree/Gurkov2019_selection_18perspecies_trimmed_5prime_Lefe_noout.fa.mldist", skip = 1, row.names = 1)
# names(disttable) <- row.names(disttable)
# disttable_triangular <- lower.triangle(as.matrix(disttable))
# 
# disstable_long <- melt(disttable_triangular)
# 
# disstable_long <- disstable_long[disstable_long$value > 0, ]
# disstable_long$first <- unlist(sapply(strsplit(x = as.character(disstable_long$Var1), split = "_"), FUN = function(x) {x[length(x)]}))
# disstable_long$second <- unlist(sapply(strsplit(x = as.character(disstable_long$Var2), split = "_"), FUN = function(x) {x[length(x)]}))
# 
# disstable_long$group <- paste(substr(disstable_long$first, 4,4), substr(disstable_long$second,4,4), sep = " vs. ")
# ## remove intraspecific differences
# disstable_long <- disstable_long[!(disstable_long$group %in% c("W vs. W", "S vs. S", "E vs. E")), ]
# 
# ggplot(disstable_long, aes(x=group, y=value)) +
#   geom_jitter() + 
#   geom_hline(yintercept = 0.16, linetype = "dotted") + ylab("GTR+I+G pairwise") +
#   expand_limits(y=c(0, 0.3)) + 
#   theme_bw()
######


## p-distance

pdist <- plot_distances("mega/pdist.csv") + ggtitle("p-distance")
k2pdist <- plot_distances("mega/K2P.csv") + ggtitle("K2P")

#plot_distances("mega/Ndiffs.csv")

ggarrange(patrdist, pdist, k2pdist, nrow = 1)
ggsave("nucl_divergence.svg", width=6, height=2.5, device = svg)
