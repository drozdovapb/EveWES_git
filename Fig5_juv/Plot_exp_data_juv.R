source("../Fig3_ampl_sexes/settings.R")

ggplot(expdat, aes(x = Date, y=Juveniles, col = Cross)) + 
  #geom_line(size = 1, alpha = 0.75) +  ##linetype = Cross ## lines just don't fit in here, as the juveniles were taken out of the tanks
  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(fill = Cross), stat = "identity") + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap( ~Cross) -> pjuv


## add the numbers

panels <- data.frame(njuv = rep('', 9),
                     Cross = factor(levels(expdat$Cross), levels = c("WxW", "WxS", "WxE", 
                                                                     "SxS", "SxW", "SxE",
                                                                     "ExE", "ExS", "ExW")))

for(i in 1:9) {
  Cross <- panels$Cross[i]
  #print(Cross)
  njuv <- sum(expdat[expdat$Cross==Cross, "Juveniles"], na.rm = TRUE)
  panels$njuv[i] <- njuv
}

pjuv + geom_label(data = panels, aes(x=as.Date("2023-04-15"), y=60, label=njuv), col="grey30")



ggsave("juv.svg", width=figwidth, height=figheight, device=svg)
ggsave("juv.png", width=figwidth, height=figheight, device=png, dpi = 300)



## juveniles!
sum(expdat$Juveniles)

#ggplot(expdat, aes(x = Date, col = Cross, y = Juveniles)) + 
#  geom_line(aes(linetype = Cross), size = 1, alpha = 0.75) + 
#  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha=0.85) + 
#  expand_limits(y=c(0, 10)) + 
#  mytheme()

sum(expdat[expdat$Cross=="WxW", "Juveniles"])
sum(expdat[expdat$Cross=="SxS", "Juveniles"])
sum(expdat[expdat$Cross=="ExE", "Juveniles"])

sum(expdat[expdat$Cross=="WxS", "Juveniles"])
sum(expdat[expdat$Cross=="SxW", "Juveniles"])
sum(expdat[expdat$Cross=="ExS", "Juveniles"])
sum(expdat[expdat$Cross=="SxE", "Juveniles"])

