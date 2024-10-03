source("../Fig3_ampl_sexes/settings.R")

###### 
## first / by the same table as other figures
## eggs fallen out
## Some counting (add sums to the panels)

sum(expdat$Fallen.out.eggs, na.rm = TRUE)

panels <- data.frame(neggs = rep('', 9),
                     Cross = factor(levels(expdat$Cross), levels = c("WxW", "WxS", "WxE", 
                                                                     "SxS", "SxW", "SxE",
                                                                     "ExE", "ExS", "ExW")))
for(i in 1:9) {
  Cross <- panels$Cross[i]
  #print(Cross)
  neggs <- sum(expdat[expdat$Cross==Cross, "Fallen.out.eggs"], na.rm = TRUE)
  panels$neggs[i] <- neggs
}
panels

ggplot(expdat, aes(x = Date, col = Cross)) + 
  #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
  geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap(~Cross,   labeller = labeller(Cross=facet.labels)) + 
  geom_label(data = panels, aes(x=as.Date("2023-04-15"), y=20, label=neggs), col="grey30")


ggsave("eggs.svg", width=figwidth, height=figheight, device=svg)
ggsave("eggs.png", width=figwidth, height=figheight, device=png, dpi = 300)

###### 

eggdat <- read.xlsx("../data/Lost_egg_stages.xlsx")
eggdat$Date <- convertToDate(eggdat$Date)
eggdat$Cross <- factor(eggdat$Cross, levels = c("WxW", "WxS", "WxE", 
                                                "SxS", "SxW", "SxE",
                                                "ExE", "ExS", "ExW"))


panels <- data.frame(neggs = rep('', 9),
                     Cross = factor(levels(expdat$Cross), levels = c("WxW", "WxS", "WxE", 
                                                                     "SxS", "SxW", "SxE",
                                                                     "ExE", "ExS", "ExW")))
for(i in 1:9) {
  Cross <- panels$Cross[i]
  #print(Cross)
  neggs <- sum(expdat[expdat$Cross==Cross, "Fallen.out.eggs"], na.rm = TRUE)
  panels$neggs[i] <- neggs
}
panels

ggplot(eggdat[eggdat$Date < as.Date("2023-02-01"), ], aes(x = Date, col = Cross)) + 
  #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
  #geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  #geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
  geom_histogram(binwidth = 1, aes(fill=`Stage.Browne.et.al.`)) + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  scale_fill_brewer(palette = "heat") + 
  facet_wrap( ~Cross) #+ 
#  geom_label(data = panels, aes(x=as.Date("2023-04-15"), y=20, label=neggs), col="grey30")

mytheme2 <- function(){
  list(theme_bw(base_size = 12), 
       theme(line = element_line(size = .5, color = "lightgrey"), 
             panel.grid.major.y = element_blank(),
             #legend.position = 'NA', 
             strip.text = element_text(size=14)),
       scale_y_continuous(breaks = pretty_breaks()),
       scale_x_date(date_breaks = "1 month", date_labels = "%b"),
       scale_linetype_manual(values = ltypes))
}


ggplot(eggdat, aes(x = Date)) + #, col = Cross)) + 
  #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
  #geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  #geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
  facet_wrap( ~Cross, drop = FALSE) + 
  geom_histogram(binwidth = 1, aes(fill=S7_passed)) + 
  expand_limits(y=c(0, 10)) + 
  mytheme2() 
#  scale_fill_brewer(palette = "heat") + 
ggsave("eggs_S7.png", width=figwidth, height=figheight, device=png, dpi = 300)


ggplot(eggdat, aes(x = Date)) + #, col = Cross)) + 
  geom_point(aes(y=`#.nuclei`)) + 
  #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
  #geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  #geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
  facet_wrap( ~Cross, drop = FALSE) + 
#  geom_histogram(binwidth = 1, aes(fill=S7_passed)) + 
  expand_limits(y=c(0, 10)) + 
  mytheme2() 
#  scale_fill_brewer(palette = "heat") + 
ggsave("eggs_nuclei.png", width=figwidth, height=figheight, device=png, dpi = 300)




##### 
## code for plotting separate graphs for each (meaningful) cross combination

# ###WxW
# ggplot(expdat[expdat$Cross=="WxW", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(fill = "grey20", aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(fill="grey20", col="grey20", aes(y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# #  facet_wrap( ~Cross) + 
# #  geom_label(data = panels, aes(x=as.Date("2023-04-15"), y=20, label=neggs), col="grey30")
# 
# 
# ggplot(expdat[expdat$Cross=="WxS", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(fill = "grey20", aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(fill="grey20", col="grey20", aes(y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# 
# ggplot(expdat[expdat$Cross=="SxW", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(fill = "grey20", aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(fill="grey20", col="grey20", aes(y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# 
# ggplot(expdat[expdat$Cross=="SxW", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# ggplot(expdat[expdat$Cross=="SxS", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# ggplot(expdat[expdat$Cross=="SxS", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# 
# ggplot(expdat[expdat$Cross=="ExE", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# ggplot(expdat[expdat$Cross=="ExS", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 
# 
# ggplot(expdat[expdat$Cross=="WxE", ], aes(x = Date, col = Cross)) + 
#   #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
#   geom_point(aes(fill = Cross, y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
#   geom_bar(aes(fill = Cross, y = `Fallen.out.eggs`), stat="identity") + 
#   expand_limits(y=c(0, 10)) + 
#   mytheme() #+ 


## For suppl


ggplot(expdat[expdat$Cross == "WxW" & expdat$Date < as.Date("2023-02-01"), ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 
ggsave("WxW.svg", device = svg, width=8, height=5)

ggplot(expdat[expdat$Cross == "SxS", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 

ggplot(expdat[expdat$Cross == "ExE", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 


ggplot(expdat[expdat$Cross == "WxS", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 


ggplot(expdat[expdat$Cross == "WxE", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 


ggplot(expdat[expdat$Cross == "SxW", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 

ggplot(expdat[expdat$Cross == "ExS", ], aes(x = Date)) + 
  geom_point(aes(y = `Fallen.out.eggs`), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(y = `Fallen.out.eggs`), stat="identity") + 
  ylab("Lost eggs") + 
  expand_limits(y=c(0, 40)) + 
  mytheme() 
