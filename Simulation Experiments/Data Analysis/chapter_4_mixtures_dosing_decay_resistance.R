library(devtools)
load_all()
library(ggplot2)
library(dplyr)
library(patchwork)


##Read in the datasets:::

set.1 = read.csv(".//mixtures.truncation.set.1.csv")
set.2 = read.csv(".//mixtures.truncation.set.2.csv")
set.3 = read.csv(".//mixtures.truncation.set.3.csv")
set.4 = read.csv(".//mixtures.truncation.set.4.csv")
set.5 = read.csv(".//mixtures.truncation.set.5.csv")
set.6 = read.csv(".//mixtures.truncation.set.6.csv")
set.7 = read.csv(".//mixtures.truncation.set.7.csv")
set.8 = read.csv(".//mixtures.truncation.set.8.csv")
set.9 = read.csv(".//mixtures.truncation.set.9.csv")
set.10 = read.csv(".//mixtures.truncation.set.10.csv")


truncation.scaled.df = rbind(set.1,
                             set.2,
                             set.3,
                             set.4,
                             set.5,
                             set.6,
                             set.7,
                             set.8,
                             set.9,
                             set.10)
#Remove to clear space
rm(set.1,
   set.2,
   set.3,
   set.4,
   set.5,
   set.6,
   set.7,
   set.8,
   set.9,
   set.10)


##Issue with the naming of the variables --> make sure it is just where threshold gens are the same for all sims
truncation.scaled.df = subset(truncation.scaled.df, threshold.gens == threshold.gens.1)


set1.75 = read.csv("mixtures.truncation.75efficacy.set.1.csv")
set2.75 = read.csv("mixtures.truncation.75efficacy.set.2.csv")
set3.75 = read.csv("mixtures.truncation.75efficacy.set.3.csv")
set4.75 = read.csv("mixtures.truncation.75efficacy.set.4.csv")
set5.75 = read.csv("mixtures.truncation.75efficacy.set.5.csv")
set6.75 = read.csv("mixtures.truncation.75efficacy.set.6.csv")
set7.75 = read.csv("mixtures.truncation.75efficacy.set.7.csv")
set8.75 = read.csv("mixtures.truncation.75efficacy.set.8.csv")
set9.75 = read.csv("mixtures.truncation.75efficacy.set.9.csv")
set10.75 = read.csv("mixtures.truncation.75efficacy.set.10.csv")

set.75s = rbind(set1.75,
                set2.75,
                set3.75,
                set4.75,
                set5.75,
                set6.75,
                set7.75,
                set8.75,
                set9.75,
                set10.75)
rm(set1.75,
   set2.75,
   set3.75,
   set4.75,
   set5.75,
   set6.75,
   set7.75,
   set8.75,
   set9.75,
   set10.75)


truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                           dose.2 == 1)


set.75s$novel.solo = truncation.df.1$novel.solo
set.75s$pyrethroid.solo = truncation.df.1$pyrethroid.solo

#add solo columns

#make one dataset
truncation.scaled.df = dplyr::bind_rows(truncation.scaled.df,
                                        set.75s)

rm(set.75s, truncation.df, truncation.df.1)


#Raw change between the mixture and the solo.
truncation.scaled.df$change.novel.peak = truncation.scaled.df$novel.solo - truncation.scaled.df$mixture.novel
truncation.scaled.df$change.pyrethroid.peak = truncation.scaled.df$pyrethroid.solo - truncation.scaled.df$mixture.pyrethroid


#Convert PRS scores to Bioassay Survival
truncation.scaled.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                               trait.mean = truncation.scaled.df$start.resistance.2,
                                                                                               half.population.bioassay.survival.resistance = 900,
                                                                                               michaelis.menten.slope = 1)*100

truncation.scaled.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.solo,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)


truncation.scaled.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                   trait.mean = truncation.scaled.df$pyrethroid.solo,
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   michaelis.menten.slope = 1)



truncation.scaled.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                             trait.mean = truncation.scaled.df$mixture.novel,
                                                                                             half.population.bioassay.survival.resistance = 900,
                                                                                             michaelis.menten.slope = 1)



truncation.scaled.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                  trait.mean = truncation.scaled.df$mixture.pyrethroid,
                                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                                  michaelis.menten.slope = 1)



#Changes in the bioassay survival mixture vs solo
truncation.scaled.df$change.novel.peak.bioassay = truncation.scaled.df$novel.solo.peak.bioassay - truncation.scaled.df$novel.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.peak.bioassay = truncation.scaled.df$pyrethroid.solo.peak.bioassay - truncation.scaled.df$pyrethroid.mix.peak.bioassay


#Categorise the dosing strategies;  Novel_Pyrethroid
#FD --> Full Dose
# HD --> Half dose (retains 50% efficacy of full dose)
truncation.scaled.df$dosing.strategy = factor(ifelse(truncation.scaled.df$dose.1 == 1 &
                                                       truncation.scaled.df$dose.2 == 1,
                                                     yes = "FD_FD",
                                                     no = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                                   truncation.scaled.df$dose.2 == 0.5,
                                                                 yes = "FD_HD",
                                                                 no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                               truncation.scaled.df$dose.2 == 1,
                                                                             yes = "HD_FD",
                                                                             no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                                           truncation.scaled.df$dose.2 == 0.5,
                                                                                         yes = "HD_HD retains 50%",
                                                                                         no = "HD_HD retains 75%")))))






#put dosings into a more logical order for plotting etc
truncation.scaled.df$dosing.strategy = factor(truncation.scaled.df$dosing.strategy,
                                              levels = c("FD_FD",
                                                         "HD_HD retains 75%",
                                                         "HD_HD retains 50%",
                                                         "FD_HD",
                                                         "HD_FD"))

table(truncation.scaled.df$dosing.strategy)


#Turn decay rates into categories for easier interpretation
truncation.scaled.df$decay.rate = factor(ifelse(truncation.scaled.df$base.decay.1 == 0.025 &
                                                  truncation.scaled.df$base.decay.2 == 0.005,
                                                yes = "much faster",
                                                no = ifelse(truncation.scaled.df$base.decay.1 == 0.025 &
                                                              truncation.scaled.df$base.decay.2 == 0.015,
                                                            yes = "faster",
                                                            no = ifelse(truncation.scaled.df$base.decay.1 == 0.015 &
                                                                          truncation.scaled.df$base.decay.2 == 0.005,
                                                                        yes = "faster",
                                                                        no = ifelse(truncation.scaled.df$base.decay.1 == 0.005 &
                                                                                      truncation.scaled.df$base.decay.2 == 0.025,
                                                                                    yes = "much slower",
                                                                                    no = ifelse(truncation.scaled.df$base.decay.1 == 0.005 &
                                                                                                  truncation.scaled.df$base.decay.2 == 0.015,
                                                                                                yes = "slower",
                                                                                                no = ifelse(truncation.scaled.df$base.decay.1 == 0.015 &
                                                                                                              truncation.scaled.df$base.decay.2 == 0.025,
                                                                                                            yes = "slower",
                                                                                                            no = ifelse(truncation.scaled.df$base.decay.1 == truncation.scaled.df$base.decay.2,
                                                                                                                        yes = "same",
                                                                                                                        no = NA))))))), levels = c("much faster", "faster", "same", "slower", "much slower"))




#calculate changes in rates of evolution:::
truncation.scaled.df$novel.solo.rate = truncation.scaled.df$novel.solo.peak.bioassay/200
truncation.scaled.df$pyrethroid.solo.rate = (truncation.scaled.df$pyrethroid.solo.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200
truncation.scaled.df$novel.mix.rate = truncation.scaled.df$novel.mix.peak.bioassay/200
truncation.scaled.df$pyrethroid.mix.rate = (truncation.scaled.df$pyrethroid.mix.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200

truncation.scaled.df$rate.change.novel.percent = ((truncation.scaled.df$novel.mix.rate - truncation.scaled.df$novel.solo.rate)/truncation.scaled.df$novel.solo.rate) * 100
truncation.scaled.df$rate.change.pyrethroid.percent = ((truncation.scaled.df$pyrethroid.mix.rate - truncation.scaled.df$pyrethroid.solo.rate)/truncation.scaled.df$pyrethroid.solo.rate) * 100


#First simply look at the impact of only Dosing / Resistance / Decay

truncation.scaled.df$novel.rate.change = ifelse(truncation.scaled.df$rate.change.novel.percent > 0,
                                                yes = "faster",
                                                no = ifelse(truncation.scaled.df$rate.change.novel.percent < 0,
                                                            yes = "slower",
                                                            no = "no change"))

truncation.scaled.df$pyrethroid.rate.change = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent > 0,
                                                     yes = "faster",
                                                     no = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent < 0,
                                                                 yes = "slower",
                                                                 no = "no change"))



##Importance of Dosing
novel.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(dosing.strategy~.)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

dosing.plot.pyr = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                 fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  facet_grid(dosing.strategy~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

dosing.plot.novel + dosing.plot.pyr

rm(dosing.plot.novel, dosing.plot.pyr)



#Importance of Resistance
novel.median.value = truncation.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = truncation.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = truncation.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = truncation.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.plot.novel = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.plot.pyr = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                 fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.plot.novel + resistance.plot.pyr
rm(resistance.plot.novel, resistance.plot.pyr)

#Importance of Decay
novel.median.value = truncation.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = truncation.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = truncation.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = truncation.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))
decay.plot.novel = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(decay.rate ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

decay.plot.pyr = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                 fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(decay.rate ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


decay.plot.novel + decay.plot.pyr
rm(decay.plot.novel, decay.plot.pyr)



#Of course there will be interactions between all these aspects.
#Dosing and Resistance

novel.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.dose.plot.novel = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                    fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.dose.plot.pr = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                 fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.dose.plot.novel + resistance.dose.plot.pr
rm(resistance.dose.plot.novel, resistance.dose.plot.pr)


novel.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy, decay.rate, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy, decay.rate, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))

pyrethroid.median.value = truncation.scaled.df %>%
  group_by(dosing.strategy, decay.rate, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = truncation.scaled.df %>%
  group_by(dosing.strategy, decay.rate, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))


resistance.dose.decay.plot.novel = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median,
                                           colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
  geom_vline(data= novel.mean.value, aes(xintercept = mean,
                                         colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dotted")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
   facet_grid(decay.rate~start.bioassay.pyrethroid)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


resistance.dose.decay.plot.pyrethroid = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                                    fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median,
                                           colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean,
                                         colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dotted")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(decay.rate~start.bioassay.pyrethroid)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

resistance.dose.decay.plot.novel + resistance.dose.decay.plot.pyrethroid

rm(resistance.dose.decay.plot.novel, resistance.dose.decay.plot.pyrethroid)


#####
truncation.scaled.df$change.novel.peak = truncation.scaled.df$novel.solo - truncation.scaled.df$mixture.novel
truncation.scaled.df$change.pyrethroid.peak = truncation.scaled.df$pyrethroid.solo - truncation.scaled.df$mixture.pyrethroid

##Convert PRS to Bioassays
truncation.scaled.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                               trait.mean = truncation.scaled.df$start.resistance.2,
                                                                                               half.population.bioassay.survival.resistance = 900,
                                                                                               michaelis.menten.slope = 1)*100

truncation.scaled.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.solo,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)


truncation.scaled.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                   trait.mean = truncation.scaled.df$pyrethroid.solo,
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   michaelis.menten.slope = 1)



truncation.scaled.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                             trait.mean = truncation.scaled.df$mixture.novel,
                                                                                             half.population.bioassay.survival.resistance = 900,
                                                                                             michaelis.menten.slope = 1)



truncation.scaled.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                  trait.mean = truncation.scaled.df$mixture.pyrethroid,
                                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                                  michaelis.menten.slope = 1)



#Calculate Differences between solo and mixture deployments
truncation.scaled.df$change.novel.peak.bioassay = truncation.scaled.df$novel.solo.peak.bioassay - truncation.scaled.df$novel.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.peak.bioassay = truncation.scaled.df$pyrethroid.solo.peak.bioassay - truncation.scaled.df$pyrethroid.mix.peak.bioassay

#calculate changes in rates of evolution:::
truncation.scaled.df$novel.solo.rate = truncation.scaled.df$novel.solo.peak.bioassay/200
truncation.scaled.df$pyrethroid.solo.rate = (truncation.scaled.df$pyrethroid.solo.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200
truncation.scaled.df$novel.mix.rate = truncation.scaled.df$novel.mix.peak.bioassay/200
truncation.scaled.df$pyrethroid.mix.rate = (truncation.scaled.df$pyrethroid.mix.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200

truncation.scaled.df$rate.change.novel.percent = ((truncation.scaled.df$novel.mix.rate - truncation.scaled.df$novel.solo.rate)/truncation.scaled.df$novel.solo.rate) * 100
truncation.scaled.df$rate.change.pyrethroid.percent = ((truncation.scaled.df$pyrethroid.mix.rate - truncation.scaled.df$pyrethroid.solo.rate)/truncation.scaled.df$pyrethroid.solo.rate) * 100

truncation.scaled.df$novel.rate.change = ifelse(truncation.scaled.df$rate.change.novel.percent > 0,
                                                yes = "faster",
                                                no = ifelse(truncation.scaled.df$rate.change.novel.percent < 0,
                                                            yes = "slower",
                                                            no = "no change"))

truncation.scaled.df$pyrethroid.rate.change = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent > 0,
                                                     yes = "faster",
                                                     no = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent < 0,
                                                                 yes = "slower",
                                                                 no = "no change"))


truncation.scaled.df$diff.end.bioassay.pyr.mix = (truncation.scaled.df$pyrethroid.mix.peak.bioassay*100) - truncation.scaled.df$start.bioassay.pyrethroid

truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                           dose.2 == 1)

truncation.df.0.75 = subset(truncation.scaled.df, dose.1 == 0.75 &
                              dose.2 == 0.75)

truncation.df.0.5 = subset(truncation.scaled.df, dose.1 == 0.5 &
                             dose.2 == 0.5)



truncation.df.1$novel_75_vs_100 = ((truncation.df.0.75$novel.mix.peak.bioassay - truncation.df.1$novel.mix.peak.bioassay) / truncation.df.1$novel.mix.peak.bioassay)*100
truncation.df.1$novel_50_vs_100 = ((truncation.df.0.5$novel.mix.peak.bioassay - truncation.df.1$novel.mix.peak.bioassay) / truncation.df.1$novel.mix.peak.bioassay)*100
truncation.df.1$novel_50_vs_75 = ((truncation.df.0.5$novel.mix.peak.bioassay - truncation.df.0.75$novel.mix.peak.bioassay) / truncation.df.0.75$novel.mix.peak.bioassay)*100

truncation.df.1$novel_75_vs_100_outcome = ifelse(truncation.df.1$novel_75_vs_100 > 0,
                                                 yes = "75% Win",
                                                 no = "100% Win")

truncation.df.1$novel_50_vs_100_outcome = ifelse(truncation.df.1$novel_50_vs_100 > 0,
                                                 yes = "50% Win",
                                                 no = "100% Win")


truncation.df.1$novel_50_vs_75_outcome = ifelse(truncation.df.1$novel_50_vs_75 > 0,
                                                yes = "50% Win",
                                                no = "75% Win")




truncation.df.1$pyr_75_vs_100 = ((truncation.df.0.75$diff.end.bioassay.pyr.mix - truncation.df.1$diff.end.bioassay.pyr.mix) / truncation.df.1$diff.end.bioassay.pyr.mix)*100
truncation.df.1$pyr_50_vs_100 = ((truncation.df.0.5$diff.end.bioassay.pyr.mix - truncation.df.1$diff.end.bioassay.pyr.mix) / truncation.df.1$diff.end.bioassay.pyr.mix)*100
truncation.df.1$pyr_50_vs_75 = ((truncation.df.0.5$diff.end.bioassay.pyr.mix - truncation.df.0.75$diff.end.bioassay.pyr.mix) / truncation.df.0.75$diff.end.bioassay.pyr.mix)*100

truncation.df.1$pyr_75_vs_100_outcome = ifelse(truncation.df.1$pyr_75_vs_100 > 0,
                                               yes = "75% Win",
                                               no = "100% Win")

truncation.df.1$pyr_50_vs_100_outcome = ifelse(truncation.df.1$pyr_50_vs_100 > 0,
                                               yes = "50% Win",
                                               no = "100% Win")


truncation.df.1$pyr_50_vs_75_outcome = ifelse(truncation.df.1$pyr_50_vs_75 > 0,
                                              yes = "50% Win",
                                              no = "75% Win")



truncation.df.1$total_75_vs_100 = (((truncation.df.0.75$diff.end.bioassay.pyr.mix + truncation.df.0.75$novel.mix.peak.bioassay)- (truncation.df.1$novel.mix.peak.bioassay +truncation.df.1$diff.end.bioassay.pyr.mix)) / (truncation.df.1$novel.mix.peak.bioassay +truncation.df.1$diff.end.bioassay.pyr.mix))*100
truncation.df.1$total_50_vs_100 = (((truncation.df.0.5$diff.end.bioassay.pyr.mix + truncation.df.0.5$novel.mix.peak.bioassay)- (truncation.df.1$novel.mix.peak.bioassay +truncation.df.1$diff.end.bioassay.pyr.mix)) / (truncation.df.1$novel.mix.peak.bioassay +truncation.df.1$diff.end.bioassay.pyr.mix))*100
truncation.df.1$total_50_vs_75 = (((truncation.df.0.5$diff.end.bioassay.pyr.mix + truncation.df.0.5$novel.mix.peak.bioassay)- (truncation.df.0.75$novel.mix.peak.bioassay +truncation.df.0.75$diff.end.bioassay.pyr.mix)) / (truncation.df.0.75$novel.mix.peak.bioassay +truncation.df.0.75$diff.end.bioassay.pyr.mix))*100


truncation.df.1$total_75_vs_100_outcome = ifelse(truncation.df.1$total_75_vs_100 > 0,
                                                 yes = "75% Win",
                                                 no = "100% Win")

truncation.df.1$total_50_vs_100_outcome = ifelse(truncation.df.1$total_50_vs_100 > 0,
                                                 yes = "50% Win",
                                                 no = "100% Win")


truncation.df.1$total_50_vs_75_outcome = ifelse(truncation.df.1$total_50_vs_75 > 0,
                                                yes = "50% Win",
                                                no = "75% Win")



#Colour Scheme:
#   "#e41a1c", #red = FD_FD
#   "#ff7f00",#orange = HD_HD 75%
#   "#984ea3", #purple = HD_HD 50%


novel_75_vs_100_plot = ggplot(truncation.df.1, aes(x=novel_75_vs_100,
                                                   fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")


novel_50_vs_100_plot = ggplot(truncation.df.1, aes(x=novel_50_vs_100,
                                                   fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

novel_50_vs_75_plot= ggplot(truncation.df.1, aes(x=novel_50_vs_75,
                                                 fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_75_vs_100_plot = ggplot(truncation.df.1, aes(x=pyr_75_vs_100,
                                                 fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_100_plot = ggplot(truncation.df.1, aes(x=pyr_50_vs_100,
                                                 fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_75_plot= ggplot(truncation.df.1, aes(x=pyr_50_vs_75,
                                               fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_75_vs_100_plot = ggplot(truncation.df.1, aes(x=total_75_vs_100,
                                                   fill = total_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_100_plot = ggplot(truncation.df.1, aes(x=total_50_vs_100,
                                                   fill = total_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_75_plot= ggplot(truncation.df.1, aes(x=total_50_vs_75,
                                                 fill = total_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

library(patchwork)

plot.layout = "
ADG
BEH
CFI
"


novel_75_vs_100_plot +
  novel_50_vs_100_plot +
  novel_50_vs_75_plot +
  pyr_75_vs_100_plot +
  pyr_50_vs_100_plot +
  pyr_50_vs_75_plot +
  total_75_vs_100_plot +
  total_50_vs_100_plot +
  total_50_vs_75_plot+
  plot_layout(design = plot.layout)+
  plot_annotation(title = "polytruncate")

rm(novel_75_vs_100_plot,
   novel_50_vs_100_plot,
   novel_50_vs_75_plot,
   pyr_75_vs_100_plot,
   pyr_50_vs_100_plot,
   pyr_50_vs_75_plot,
   total_75_vs_100_plot,
   total_50_vs_100_plot,
   total_50_vs_75_plot)





###Fit and Plot Generalised Additive Models:
# 1.Heritability
# 2.Female Exposure
# 3. Total Male Exposure
# 4. Coverage
# 5. Dispersal

#Heritability
heritability.novel.gam = ggplot(truncation.scaled.df, aes(x=heritability,
                                 y=rate.change.novel.percent,
                                 colour = dosing.strategy,
                                 fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Heritability")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=heritability,
                                                          y=rate.change.pyrethroid.percent,
                                                          colour = dosing.strategy,
                                                          fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Heritability")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



heritability.novel.gam + heritability.pyrethroid.gam
rm(heritability.novel.gam, heritability.pyrethroid.gam)

female.exposure.novel.gam = ggplot(truncation.scaled.df, aes(x=female.exposure,
                                                          y=rate.change.novel.percent,
                                                          colour = dosing.strategy,
                                                          fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

female.exposure.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=female.exposure,
                                                               y=rate.change.pyrethroid.percent,
                                                               colour = dosing.strategy,
                                                               fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



female.exposure.novel.gam + female.exposure.pyrethroid.gam
rm(female.exposure.novel.gam, female.exposure.pyrethroid.gam)

male.exposure.novel.gam = ggplot(truncation.scaled.df, aes(x=male.exposure,
                                                             y=rate.change.novel.percent,
                                                             colour = dosing.strategy,
                                                             fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Male Insecticide Encounter Probability")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

male.exposure.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=male.exposure,
                                                                  y=rate.change.pyrethroid.percent,
                                                                  colour = dosing.strategy,
                                                                  fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Male Insecticide Encounter Probability")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



male.exposure.novel.gam + male.exposure.pyrethroid.gam
rm(male.exposure.novel.gam, male.exposure.pyrethroid.gam)


coverage.novel.gam = ggplot(truncation.scaled.df, aes(x=intervention.coverage,
                                                           y=rate.change.novel.percent,
                                                           colour = dosing.strategy,
                                                           fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Intervention Coverage")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

coverage.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=intervention.coverage,
                                                                y=rate.change.pyrethroid.percent,
                                                                colour = dosing.strategy,
                                                                fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Intervention Coverage")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


coverage.novel.gam + coverage.pyrethroid.gam
rm(coverage.novel.gam, coverage.pyrethroid.gam)


dispersal.novel.gam = ggplot(truncation.scaled.df, aes(x=dispersal,
                                                      y=rate.change.novel.percent,
                                                      colour = dosing.strategy,
                                                      fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Dispersal Rate")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

dispersal.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=dispersal,
                                                           y=rate.change.pyrethroid.percent,
                                                           colour = dosing.strategy,
                                                           fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  xlab("Dispersal Rate")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



dispersal.novel.gam + dispersal.pyrethroid.gam
rm(dispersal.novel.gam, dispersal.pyrethroid.gam)








