source("../Fig3_ampl_sexes/settings.R")


## plot everything including zeroes
ggplot(expdat, aes(x = Date, y=Juveniles, col = Cross)) + 
  #geom_line(size = 1, alpha = 0.75) +  ##linetype = Cross ## lines just don't fit in here, as the juveniles were taken out of the tanks
  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(fill = Cross), stat = "identity") + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap(~Cross,   labeller = labeller(Cross=facet.labels)) -> pjuv

#plot only the period when the juveniles emerged ##nope, it's not a good idea
ggplot(expdat[expdat$Juveniles > 0,], aes(x = Date, y=Juveniles, col = Cross)) + 
  #geom_line(size = 1, alpha = 0.75) +  ##linetype = Cross ## lines just don't fit in here, as the juveniles were taken out of the tanks
  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(fill = Cross), stat = "identity") + 
  mytheme() + 
  facet_wrap(~Cross,   labeller = labeller(Cross=facet.labels)) -> pjuv

## do not plot zeroes
expdat[expdat$Juveniles == 0, "Juveniles"] <- NA
ggplot(expdat, aes(x = Date, y=Juveniles, col = Cross)) + 
  #geom_line(size = 1, alpha = 0.75) +  ##linetype = Cross ## lines just don't fit in here, as the juveniles were taken out of the tanks
  geom_point(aes(fill = Cross), shape = 21, size = 2, alpha = 0.85) + 
  geom_bar(aes(fill = Cross), stat = "identity") + 
  expand_limits(y=c(0, 10), x=c(min(expdat$Date), max(expdat$Date))) + 
  facet_wrap(~Cross,   labeller = labeller(Cross=facet.labels), drop = FALSE) + 
  mytheme() -> pjuv
pjuv

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

panels$njuv[5] <- "101** \nGenotyped: 24 S"
panels$njuv[2] <- "1* \nGenotyped: 1 S"
panels$njuv[8] <- "7 \nGenotyped: 7 hybrid"


pjuv + geom_label(data = panels, label.padding = unit(0.1, "lines"), label.size=0.1,
                  aes(x=as.Date("2023-03-12"), y=70, label=njuv), 
                  col="grey30")



ggsave("Fig5_juv.svg", width=figwidth, height=figheight, device=svg)
ggsave("Fig5_juv.png", width=figwidth, height=figheight, device=png, dpi = 300)



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

## median
expdat$Day
expdatW <- expdat[expdat$Cross=="WxW",]
median(rep(expdatW$Date, expdatW$Juveniles))
median(rep(expdatW$Day, expdatW$Juveniles))
expdatS <- expdat[expdat$Cross=="SxS",]
median(rep(expdatS$Date, expdatS$Juveniles))
median(rep(expdatS$Day, expdatS$Juveniles))
expdatE <- expdat[expdat$Cross=="ExE",]
median(rep(expdatE$Date, expdatE$Juveniles))
median(rep(expdatE$Day, expdatE$Juveniles))
