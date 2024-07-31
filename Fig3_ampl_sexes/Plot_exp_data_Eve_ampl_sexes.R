source("./settings.R")

ggplot(expdat, aes(x = Date, col = Cross)) + 
  geom_line(aes(y = `Amplexuses`), size = 1, alpha = 0.75) +  ##linetype = Cross
  geom_point(aes(fill = Cross, y = `Amplexuses`), shape = 21, size = 2, alpha = 0.85) + 
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  facet_wrap( ~Cross)


### amplexuses AND sexes for the presentation
expdat$animalsInAmplexus <- expdat$Amplexuses*2 ## not used finally

facet.labels <- c("♀W × ♂W", "♀W × ♂ S", "♀W × ♂E", 
           "♀S × ♂S", "♀S × ♂W", "♀S × ♂E",
           "♀E × ♂E", "♀E × ♂S", "♀E × ♂W")
names(facet.labels) <- c("WxW", "WxS", "WxE", "SxS", "SxW", "SxE", "ExE", "ExS", "ExW")

expdat$isE <- expdat$Cross %in% c("WxE", "SxE", "ExS", "ExW")

#expdat$females <- expdat$females[c(T, F)]

femshapes <- rep(c(rep("\U2640", 9), rep("", 9), rep("", 9)), 18)
femshapes[478:486] <- "\U2640"
maleshapes <- rep(c(rep("\U2642", 9), rep("", 9), rep("", 9)), 18)
maleshapes[478:486] <- "\U2642"

ggplot(expdat, aes(x = Date)) + 
  geom_rect(data = expdat[1:9,], aes(fill=isE), xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10, alpha=0.3) + 
  geom_line(aes(y = Amplexuses, col = Cross), size = 1, alpha = 0.75) +  ##linetype = Cross
  geom_point(aes(y = Amplexuses, col = Cross), shape = 21, size = 1, alpha = 0.85) + #fill = Cross,
  geom_area(aes(y = Amplexuses), fill="grey90", alpha = 0.85) +
  geom_point(aes(y=females), shape = femshapes, col="#FF33CC", size = 5, alpha = 1) + #fill = Cross #"\U2640"
  geom_line(aes(y=females), col="#FF33CC", alpha = 1) + #fill = Cross
  geom_point(aes(y=males), shape = maleshapes, col="#0066FF", size = 5, alpha = 1)  + #fill = Cross #"\U2642"
  geom_line(aes(y=males), col="#0066FF",  alpha = 1) +
  expand_limits(y=c(0, 10)) + 
  mytheme() + 
  ylab("") + 
  facet_wrap(~Cross,   labeller = labeller(Cross=facet.labels))  +
  theme(legend.position = 'none', strip.text = element_text(size=12, face="plain")) +#, strip.background = element_rect(fill="white")) + 
  scale_fill_manual(values = c("white", "pink"))
 #fill = Cross

ggsave("ampl_sexes.svg", width=figwidth, height=figheight, device=svg)
ggsave("ampl_sexes.png", width=figwidth, height=figheight, device=png, dpi = 300)

