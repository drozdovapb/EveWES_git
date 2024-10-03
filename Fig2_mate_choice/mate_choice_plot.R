library(openxlsx)
library(ggplot2)
#devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

mate.choice.tbl <- read.xlsx("../data/mate_choice_exp.xlsx", sheet = "sorted")

mate.choice.tbl$Result <- factor(mate.choice.tbl$Result, levels = c("FALSE", "TRUE", "ND"))

ggplot(mate.choice.tbl) + geom_mosaic(aes(x = Type, fill = Result)) +
  coord_flip() 


ggplot(mate.choice.tbl) + geom_bar(aes(x = Type, fill = Result), position = "stack") + 
  coord_flip() + 
  theme_bw(base_size = 14) + theme(panel.grid.minor.x = element_blank())
#ggsave("mate_choice_draft.png")
ggsave("mate_choice_draft.svg")


