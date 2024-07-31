Sys.setlocale("LC_TIME", "C")
library(ggplot2)
library(openxlsx)
library(scales) ## for different linetypes
library(ggpubr) ## for ggarrange

## tune ggplot theme

## scale_color_manual(values=c(
## "#000000", "#E69F00", "#56B4E9", "#009E73",
## "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
## black    orange  sky_blue    green 
## yellow   blue    vermilion   reddish_purple


## my colors
#
WxW <- "#F0E442"; W <- WxW
#WxW <- "#FFC300"
SxS <- "#4477AA"; S <- SxS
ExE <- "#D81B60"; E <- ExE
WxS <- rgb(t(round(0.5*(col2rgb(W)+col2rgb(S)))), maxColorValue = 255)
ExS <- rgb(t(round(0.5*(col2rgb(E)+col2rgb(S)))), maxColorValue = 255)
ExW <- rgb(t(round(0.5*(col2rgb(W)+col2rgb(E)))), maxColorValue = 255)


fillcolors <- c(W, W, W, S, S, S, E, E, E)
colcolors <- c(WxW, WxS, ExW, SxS, WxS, ExS, ExE, ExS, ExW)
ltypes <- c("dashed", "solid", "solid", "solid", "dashed", "solid", "solid", "solid", "dashed")


mytheme <- function(){
  list(theme_bw(base_size = 12), 
       theme(line = element_line(size = .5, color = "lightgrey"), 
             panel.grid.major.y = element_blank(),
             legend.position = 'NA'),
       scale_fill_manual(values = fillcolors),
       scale_color_manual(values = colcolors),
       scale_y_continuous(breaks = pretty_breaks()),
       scale_x_date(date_breaks = "1 month", date_labels = "%b"),
       scale_linetype_manual(values = ltypes))
}
## not here but might be useful...
##theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

## plot settings
figwidth = 8
figheight = 6

## read the data
expdat <- read.xlsx("../data/Experiment_table_EveWES.xlsx")
expdat$Date <- convertToDate(expdat$Date)
expdat$Cross <- factor(expdat$Cross, levels = c("WxW", "WxS", "WxE", 
                                                "SxS", "SxW", "SxE",
                                                "ExE", "ExS", "ExW"))