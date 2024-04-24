library(patchwork)
library(devtools)
load_all()
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
truncation.df = dplyr::bind_rows(truncation.scaled.df,
                     set.75s)

##have only 1:1 ; 0.75:0.75 and 0.5:0.5 simulations

truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                          dose.2 == 1)

truncation.df.0.75 = subset(truncation.df, dose.1 == 0.75 &
                          dose.2 == 0.75)

truncation.df.0.5 = subset(truncation.df, dose.1 == 0.5 &
                          dose.2 == 0.5)




rm(set.75s)







#one dataframe
truncation.scaled.df = rbind(truncation.df.1,
                      truncation.df.0.75,
                      truncation.df.0.5)

rm(truncation.df)


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



#Define dosing strategies
truncation.scaled.df$dosing.strategy = ifelse(truncation.scaled.df$dose.1 == 1,
                                              yes = "Full Dose",
                                              no = ifelse(truncation.scaled.df$dose.1 == 0.5,
                                                          yes = "Half Dose retains 50% Efficacy",
                                                          no = "Half Dose retains 75% Efficacy"))

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


pyrethroid.plot = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                               fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "red", "blue"))+
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
  xlim(-90, 10)+
  xlab("Change in the Rate of Evolution")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  facet_grid(dosing.strategy ~ start.bioassay.pyrethroid)+
  theme_bw()+
  theme(legend.position = "none")

novel.plot = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                          fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
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
  xlim(-90, 10)+
  xlab("Change in the Rate of Evolution")+
  ggtitle("polytruncate: Novel Insecticide")+
  facet_grid(dosing.strategy ~ start.bioassay.pyrethroid)+
  theme_bw()+
  theme(legend.position = "none")


novel.plot + pyrethroid.plot
rm(novel.plot, pyrethroid.plot)


######

##gets the bioassay survival stuff

#colour scheme:
#100 =  #35978f    teal
#75 = #810f7c   purple
#50 = #b2182b   red



novel_75_vs_100_plot = ggplot(truncation.df.1, aes(x=novel_75_vs_100,
                                               fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c",
                               "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")


novel_50_vs_100_plot = ggplot(truncation.df.1, aes(x=novel_50_vs_100,
                                               fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

novel_50_vs_75_plot= ggplot(truncation.df.1, aes(x=novel_50_vs_75,
                                             fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c( "#810f7c", "#b2182b"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_75_vs_100_plot = ggplot(truncation.df.1, aes(x=pyr_75_vs_100,
                                             fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_100_plot = ggplot(truncation.df.1, aes(x=pyr_50_vs_100,
                                             fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_75_plot= ggplot(truncation.df.1, aes(x=pyr_50_vs_75,
                                           fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#b2182b"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_75_vs_100_plot = ggplot(truncation.df.1, aes(x=total_75_vs_100,
                                               fill = total_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_100_plot = ggplot(truncation.df.1, aes(x=total_50_vs_100,
                                               fill = total_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_75_plot= ggplot(truncation.df.1, aes(x=total_50_vs_75,
                                             fill = total_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#b2182b"))+
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
#colour scheme:
#100 =  #35978f    teal
#75 = #810f7c   purple
#50 = #b2182b   red



female.novel.gam = ggplot(truncation.scaled.df, aes(x=female.exposure,
                                                    y=rate.change.novel.percent,
                                                    colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polytruncate: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


female.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=female.exposure,
                                                         y=rate.change.pyrethroid.percent,
                                                         colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

female.novel.gam + female.pyrethroid.gam

rm(female.pyrethroid.gam, female.novel.gam)





heritability.novel.gam = ggplot(truncation.scaled.df, aes(x=heritability,
                                                    y=rate.change.novel.percent,
                                                    colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Heritability")+
  ggtitle("polytruncate: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=heritability,
                                                         y=rate.change.pyrethroid.percent,
                                                         colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Heritability")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

heritability.novel.gam + heritability.pyrethroid.gam

rm(heritability.novel.gam , heritability.pyrethroid.gam)




intervention.novel.gam = ggplot(truncation.scaled.df, aes(x=intervention.coverage,
                                                          y=rate.change.novel.percent,
                                                          colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Intervention Coverage")+
  ggtitle("polytruncate: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


intervention.pyrethroid.gam = ggplot(truncation.scaled.df, aes(x=heritability,
                                                               y=rate.change.pyrethroid.percent,
                                                               colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Intervention Coverage")+
  ggtitle("polytruncate: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

intervention.novel.gam + intervention.pyrethroid.gam
rm(intervention.novel.gam , intervention.pyrethroid.gam)



