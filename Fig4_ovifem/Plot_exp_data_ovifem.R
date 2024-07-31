source("../Fig3_ampl_sexes/settings.R")

ggplot(expdat, aes(x = Date, col = Cross, y = `SUM.females.w/eggs`)) + 
  #  geom_line(aes(y = `Fallen.out.eggs`), size = 1, alpha = 0.75) +  ##linetype = Cross
  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha = 0.85) + 
  geom_line(alpha=.75) + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap( ~Cross)

ggsave("ovifem.svg", width=figwidth, height=figheight, device=svg)
ggsave("ovifem.png", width=figwidth, height=figheight, device=png, dpi = 300)
