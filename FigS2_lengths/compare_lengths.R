library(ggplot2)
library(openxlsx)
library(ggpubr)

source("../Fig3_ampl_sexes/settings.R")

lengths <- read.xlsx("../data/TableS1_Lengths.xlsx")
lengths <- lengths[complete.cases(lengths$Length), ]

table(lengths$Species, lengths$Sex)

ggplot(lengths, aes(x=Species, y=Length, fill=Species, col=Sex)) +
  geom_boxplot(outlier.color = 'NA') + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1), aes(shape=Sex, col=Sex), size=2, alpha=0.75) + 
  scale_x_discrete(limits=rev) + 
  expand_limits(y=0) + 
  scale_fill_manual(values = c(E, S, W), 
                    guide = guide_legend(override.aes = list(shape = c(NA, NA, NA)))) + 
  scale_color_manual(values = c("grey30", "grey33")) + 
#  scale_shape_manual(values = c("♀", "♂")) +
#  scale_shape_manual(values = c("\U25B2", "\U25BC")) +
#  scale_shape_manual(values = c(2, 6)) +
#  scale_shape_manual(values = c(21, 24)) +
  scale_shape_manual(values = c(19, 15)) + 
  theme_bw(base_size = 14) -> p1

p1

pairwise.wilcox.test(g = lengths$Sex, x = lengths$Length)
pairwise.wilcox.test(g = lengths$Sex, x = lengths$Length)

library(tidyr)
library(dplyr)
library(rstatix)
lengths %>% #filter(!is.na(Length)) %>%
  group_by(Species) %>% 
  wilcox_test(Length ~ Sex) %>% 
  adjust_pvalue(method="holm") %>%
  add_significance("p.adj") %>%
  add_xy_position(x = "Species", dodge = 0.8) -> stat.test1 

stat.test1$y.position <- rev(stat.test1$y.position)

p1

p1 + stat_pvalue_manual(
  stat.test1,  label = "p.adj.signif", tip.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) -> p2

lengths %>% 
  wilcox_test(Length ~ Species, p.adjust.method = "holm") %>% 
  add_xy_position(x = "Species") -> stat.test2

stat.test2$x <- 1:3
stat.test2$Species <- c("W", "S", "E")
#stat.test2$y.position <- 42
stat.test2$y.position <- rev(stat.test2$y.position) + 3
p2

p2 + stat_pvalue_manual(stat.test2, label = 'p.adj.signif', tip.length = 0.02, #col="green4", 
  step.increase = 0.1) +
  ylab("Body length, mm") + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

ggsave("FigS2_lengths.png", device=png, width=8, height=4)

########
## Length vs. body angle
cmp <- read.xlsx("../data/length_vs_angle.xlsx")
ggplot(cmp, aes(x=Angle, y=Length)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  expand_limits(y=0) +
#  expand_limits(y=0, x=0) +
  theme_bw(base_size = 14)
ggsave("length_vs_angle.png", width=6, height=4, device=png)
summary(lm(cmp$Length ~ cmp$Angle))



### Data from Govorukhina, 2005 (Listvyanka)

lengthsG <- read.xlsx("../data/Lengths_Govorukhina2005.xlsx")
lengthsG$Species <- "W"

ggplot(lengthsG, aes(x=Species, y=Length, col=Sex)) +
  geom_boxplot(outlier.color = 'NA', fill=W) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2), aes(shape=Sex), size=2) + 
  scale_x_discrete(limits=rev) + 
  expand_limits(y=0) + 
  #scale_fill_manual(values = c(E, S, W)) + 
  #  scale_shape_manual(values = c("♀", "♂")) +
  scale_shape_manual(values = c(16, 15)) +
  theme_bw(base_size = 14) 

ggsave("lengthsW_G.png", width=4, height=4, device=png)

wilcox_test(lengthsG, Length ~ Sex)
