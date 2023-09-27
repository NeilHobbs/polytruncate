library(devtools)
load_all()

trait.values = create_normal_distribution(vector.length = 1000,
                           trait.mean = 0,
                           standard.deviation = 20)

values.frequency = calculate_density_of_trait_values(vector.length = 1000,
                                                     trait.mean = 0,
                                                     standard.deviation = 20)


unexposed.individuals = 0.3 * values.frequency

exposed.survivors.vhigh.efficacy = c(rep(0, 950), values.frequency[951:1000]* 0.7)
exposed.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7)
exposed.survivors.med.efficacy = c(rep(0, 500), values.frequency[501:1000]* 0.7)
exposed.survivors.low.efficacy = c(rep(0, 200), values.frequency[201:1000]* 0.7)

end.survivors.vhigh.efficacy = c(rep(0, 950), values.frequency[951:1000] * 0.7) + unexposed.individuals
end.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7) + unexposed.individuals
end.survivors.med.efficacy = c(rep(0, 500), values.frequency[501:1000]* 0.7) + unexposed.individuals
end.survivors.low.efficacy = c(rep(0, 200), values.frequency[201:1000]* 0.7) + unexposed.individuals



sum(trait.values * end.survivors.med.efficacy)/sum(end.survivors.med.efficacy)
sum(trait.values * end.survivors.high.efficacy)/sum(end.survivors.high.efficacy)



df = data.frame(exposed.survivors.vhigh.efficacy,
                exposed.survivors.high.efficacy,
                exposed.survivors.med.efficacy,
                exposed.survivors.low.efficacy,
                end.survivors.vhigh.efficacy,
                end.survivors.high.efficacy,
                end.survivors.med.efficacy,
                end.survivors.low.efficacy,
                unexposed.individuals,
                trait.values)


before.selection = ggplot(df, aes(x=trait.values,
                                  y=values.frequency))+
  geom_area(alpha = 0.7, fill = "grey")+
  geom_vline(xintercept = 0,
             size = 2,
             colour = "darkblue")+
  ylim(0, 0.02)+
  xlab("PRS")+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

unexposed.plot = ggplot(df, aes(x=trait.values,
                                y=unexposed.individuals))+
  geom_area(alpha = 0.7, fill = "lightgreen")+
  ylim(0, 0.02)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Escape Insecticide Exposure")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


low.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.low.efficacy))+
  geom_area(alpha = 0.7, fill = "pink")+
  ylim(0, 0.02)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Low Efficacy Insecticide")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


low.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.low.efficacy))+
  geom_area(alpha = 0.3, fill = "purple")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             size = 2,
             colour = "darkblue")+
  geom_vline(xintercept = (sum(trait.values * end.survivors.low.efficacy)/sum(end.survivors.low.efficacy)),
             colour = "darkred",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

med.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.med.efficacy))+
  geom_area(alpha = 0.7, fill = "pink")+
  ylim(0, 0.02)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Moderate Efficacy Insecticide")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


med.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.med.efficacy))+
  geom_area(alpha = 0.3, fill = "purple")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             size = 2,
             colour = "darkblue")+
  geom_vline(xintercept = (sum(trait.values * end.survivors.med.efficacy)/sum(end.survivors.med.efficacy)),
             colour = "darkred",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

high.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.high.efficacy))+
  geom_area(alpha = 0.7, fill = "pink")+
  ylim(0, 0.02)+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  ggtitle("High Efficacy Insecticide")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


high.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.high.efficacy))+
  geom_area(alpha = 0.3, fill = "purple")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             size = 2,
             colour = "darkblue")+
  geom_vline(xintercept = (sum(trait.values * end.survivors.high.efficacy)/sum(end.survivors.high.efficacy)),
             colour = "darkred",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

vhigh.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.vhigh.efficacy))+
  geom_area(alpha = 0.7, fill = "pink")+
  ylim(0, 0.02)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Very High Efficacy Insecticide")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


vhigh.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.vhigh.efficacy))+
  geom_area(alpha = 0.3, fill = "purple")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             size = 2,
             colour = "darkblue")+
  geom_vline(xintercept = (sum(trait.values * end.survivors.vhigh.efficacy)/sum(end.survivors.vhigh.efficacy)),
             colour = "darkred",
             size = 2)+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#Add in the  mixtures:
#low-low
#low-med
#low-high
#med-low
#med-med
#med-hihg
#high-low
#high-med
#high-high


library(patchwork)

the.layout = "
#B#
#CD
AEF
#GH
#IJ
"


before.selection +
  unexposed.plot +
  low.efficacy.plot +
  low.efficacy.end.plot +
  med.efficacy.plot +
  med.efficacy.end.plot +
  high.efficacy.plot +
  high.efficacy.end.plot +
  vhigh.efficacy.plot +
  vhigh.efficacy.end.plot +
  plot_layout(design = the.layout)







