library(ggplot2)
library(ggbeeswarm)
library(matrixcalc) #triangular matrix
library(reshape2) ##right here melt works better than with tidyverse
library(ggpubr)

## a convenience function that takes a distance table (csv) 
## and plots distances (returnts a ggplot object)
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
  # group = biological species
  table_long$group <- paste(substr(table_long$first, 4,4), substr(table_long$second,4,4), sep = "\n")
  table_long$group[table_long$group == "S\nW"] <- "W\nS"
  table_long$group[table_long$group == "E\nW"] <- "W\nE"
  table_long$group[table_long$group == "S\nE"] <- "E\nS"
  print(paste("number of comparisons", nrow(table_long))) #let's make sure we don't run the same comparisons twice
  ## remove intraspecific differences
  table_long <- table_long[!(table_long$group %in% c("W\nW", "S\nS", "E\nE")), ]
  myplot <-   ggplot(table_long, aes(x=group, y=value)) +
    geom_jitter(size=0.5) + 
    #geom_boxplot() + 
#    geom_beeswarm() + 
    expand_limits(y=c(0, 0.26)) + 
    scale_x_discrete(limits = rev) + 
    scale_y_continuous(n.breaks=6) + 
    xlab("") + ylab("") + 
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(hjust=0.5, size=14))
  return(myplot)
}

## patristic distances with patristic
## mrbayes; nope; that's not how it works. 
#patrdist <- plot_distances("mb_patristic/patristic_matrix.ed.csv", already_triangular = FALSE) + 
#  ggtitle("Patristic Bayes") + geom_hline(yintercept = 0.16, linetype = "dotted")
## iqtree
patrdist_ml <- plot_distances("COI_divergence/iqtree/iqtree_matrix.ed.csv", already_triangular = FALSE) + 
  ggtitle("Patristic ML") + geom_hline(yintercept = 0.16, linetype = "dotted")

## p-distance (uncorrected pairwise; identified with MEGA)
pdist <- plot_distances("COI_divergence/mega/pdist.csv") + ggtitle("p-distance")
## K2P distance (Kimura 2-parameter; calculated with MEGA)
k2pdist <- plot_distances("COI_divergence/mega/K2P.csv") + ggtitle("K2P")

## just in case pairwise (GTR??) distance as returned by IQ-TREE in the .mldist file
## mldist
disttable <- read.table("COI_divergence/iqtree/Gurkov2019_selection_18perspecies_trimmed_5prime_Lefe_noout.fa.mldist", skip = 1)

## mldist file from IQ-TREE is a bit funny.
## The easiest way to work with this 'table' 
names(disttable)[-1] <- disttable[,1]
write.csv(disttable, "COI_divergence/iqtree/Gurkov2019_selection_18perspecies_trimmed_5prime_Lefe_noout.fa.mldist.csv", row.names = FALSE)
mldist <- plot_distances("COI_divergence/iqtree/Gurkov2019_selection_18perspecies_trimmed_5prime_Lefe_noout.fa.mldist.csv")

#ggarrange(patrdist, pdist, k2pdist, nrow = 1)
#ggsave("nucl_divergence.svg", width=6, height=2.5, device = svg)

## mldist are even greater; not sure I need this, too
#ggarrange(patrdist_ml, mldist, pdist, k2pdist, nrow = 1)
#ggsave("nucl_divergence_4.svg", width=7.5, height=2.5, device = svg)

ggarrange(patrdist_ml, pdist, k2pdist, nrow = 1)
ggsave("nucl_divergence2.svg", width=6, height=2.5, device = svg)
