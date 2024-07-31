source("../Fig3_ampl_sexes/settings.R")


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
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap( ~Cross) + 
  geom_label(data = panels, aes(x=as.Date("2023-04-15"), y=20, label=neggs), col="grey30")


ggsave("eggs.svg", width=figwidth, height=figheight, device=svg)
ggsave("eggs.png", width=figwidth, height=figheight, device=png, dpi = 300)

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