library(devtools)
load_all()
parameter.space = read.csv(".//parameter_space_mixture_decay.csv")

heritability = list()
male.exposure = list()
female.exposure = list()
coverage = list()
dispersal = list()

for(i in 1:200){

  heritability[[i]] = rep(c(parameter.space$heritability[i]), times = 9)
  male.exposure[[i]] = rep(c(parameter.space$male.exposure[i]), times = 9)
  female.exposure[[i]] = rep(c(parameter.space$female.exposure[i]), times = 9)
  coverage[[i]] = rep(c(parameter.space$coverage[i]), times = 9)
  dispersal[[i]] = rep(c(parameter.space$dispersal[i]), times = 9)

}

heritability = unlist(heritability)
male.exposure = unlist(male.exposure)
female.exposure = unlist(female.exposure)
coverage = unlist(coverage)
dispersal = unlist(dispersal)

base.decay = rep(rep(c(0.005, 0.015, 0.025), 3), 200)
threshold.gens = rep(c(10, 10, 10, 15, 15, 15, 20, 20, 20), 200)

solo.parameter.df  = data.frame(heritability = unlist(heritability),
                                male.exposure = unlist(male.exposure),
                                female.exposure = unlist(female.exposure),
                                coverage = unlist(coverage),
                                dispersal = unlist(dispersal),
                                base.decay = rep(rep(c(0.005, 0.015, 0.025), 3), 200),
                                threshold.gens = rep(c(10, 10, 10, 15, 15, 15, 20, 20, 20), 200))


novel.solo.peak = c()
novel.solo.mean = c()

for(i in 1:1800){
  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 1,
                                                                            female.fitness.cost = 0,
                                                                            male.fitness.cost = 0,
                                                                            female.insecticide.exposure = solo.parameter.df$female.exposure[i],
                                                                            male.insecticide.exposure = solo.parameter.df$male.exposure[i],
                                                                            heritability = solo.parameter.df$heritability[i],
                                                                            dispersal.rate = solo.parameter.df$dispersal[i],
                                                                            intervention.coverage = solo.parameter.df$coverage[i],
                                                                            standard.deviation = NA,
                                                                            sd.scaled = TRUE,
                                                                            z.sd.coefficient = 0.4,
                                                                            z.sd.intercept = 18,
                                                                            vector.length = 1000,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            regression.coefficient = 0.48,
                                                                            regression.intercept = 0.15,
                                                                            maximum.generations = 200,
                                                                            irm.strategy = "sequence",
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            withdrawal.threshold.value = 1,
                                                                            return.threshold.value = 0.08,
                                                                            deployment.frequency = 30, #minimum deployment frequency
                                                                            maximum.resistance.value = 90000,
                                                                            starting.refugia.resistance.score = 0,
                                                                            starting.intervention.resistance.score = 0,
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = solo.parameter.df$threshold.gens[i],#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = solo.parameter.df$base.decay[i],
                                                                            rapid.decay.rate = 0.08,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA),
                               maximum.generations = 200, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  novel.solo.peak[i] = max(B$resistance.score)
  novel.solo.mean[i] = mean(B$resistance.score)

  if(i %% 50 == 0){print(i)}
}

solo.parameter.df$novel.solo.peak = novel.solo.peak
solo.parameter.df$novel.solo.mean = novel.solo.mean


write.csv(solo.parameter.df, ".//solo.parameter.decay.SDscaled.csv")

##Next do for mixturse::::

heritability = list()
male.exposure = list()
female.exposure = list()
coverage = list()
dispersal = list()
for(i in 1:200){

  heritability[[i]] = rep(c(parameter.space$heritability[i]), times = 1296)
  male.exposure[[i]] = rep(c(parameter.space$male.exposure[i]), times = 1296)
  female.exposure[[i]] = rep(c(parameter.space$female.exposure[i]), times = 1296)
  coverage[[i]] = rep(c(parameter.space$coverage[i]), times = 1296)
  dispersal[[i]] = rep(c(parameter.space$dispersal[i]), times = 1296)

}

heritability = unlist(heritability)
male.exposure = unlist(male.exposure)
female.exposure = unlist(female.exposure)
coverage = unlist(coverage)
dispersal = unlist(dispersal)
base.decay.1 = rep(rep(rep(c(rep(0.005, 27), rep(0.015, 27), rep(0.025, 27)), 4), 4), 200)
threshold.gens.1 = rep(rep(rep(rep(c(rep(10, 9), rep(15, 9), rep(20, 9)), 3), 4), 4), 200)
base.decay.2 = rep(rep(rep(rep(c(rep(0.005, 3), rep(0.015, 3), rep(0.025, 3)), 9), 4), 4), 200)
threshold.gens.2 = rep(rep(rep(rep(c(10, 15, 20), 27), 4), 4), 200)
start.resistance.2 = rep(c(rep(rep(0, 81), 4), rep(rep(100, 81), 4), rep(rep(900, 81), 4), rep(rep(3600, 81), 4)), 200)
dose.1 = rep(rep(c(rep(1, 81), rep(1, 81), rep(0.5, 81), rep(0.5, 81)), 4), 200)
dose.2 = rep(rep(c(rep(1, 81), rep(0.5, 81), rep(1, 81), rep(0.5, 81)), 4), 200)

mixture.parameter.df  = data.frame(heritability,
                                   male.exposure,
                                   female.exposure,
                                   coverage,
                                   dispersal,
                                   base.decay.1,
                                   threshold.gens.1,
                                   base.decay.2,
                                   threshold.gens.2,
                                   start.resistance.2,
                                   dose.1,
                                   dose.2)


novel.mix.peak = c()
novel.mix.mean = c()
pyrethroid.mix.peak = c()
pyrethroid.mix.mean = c()
for(i in 1:259200){
  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                     exposure.scaling.factor = 1,
                                                                                     female.fitness.cost = 0,
                                                                                     male.fitness.cost = 0,
                                                                                     female.insecticide.exposure = mixture.parameter.df$female.exposure[i],
                                                                                     male.insecticide.exposure = mixture.parameter.df$male.exposure[i],
                                                                                     heritability = mixture.parameter.df$heritability[i],
                                                                                     dispersal.rate = mixture.parameter.df$dispersal[i],
                                                                                     intervention.coverage = mixture.parameter.df$coverage[i],
                                                                                     standard.deviation = NA,
                                                                                     sd.scaled = TRUE,
                                                                                     z.sd.coefficient = 0.4,
                                                                                     z.sd.intercept = 18,
                                                                                     vector.length = 1000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     regression.coefficient = 0.48,
                                                                                     regression.intercept = 0.15,
                                                                                     maximum.generations = 200,
                                                                                     irm.strategy = "sequence",
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     withdrawal.threshold.value = 1,
                                                                                     return.threshold.value = 0.08,
                                                                                     deployment.frequency = 30, #minimum deployment frequency
                                                                                     maximum.resistance.value = 90000,
                                                                                     starting.refugia.resistance.score = c(0, mixture.parameter.df$start.resistance.2[i]),
                                                                                     starting.intervention.resistance.score = c(0, mixture.parameter.df$start.resistance.2[i]),
                                                                                     applied.insecticide.dose = c(mixture.parameter.df$dose.1[i], mixture.parameter.df$dose.2[i]),
                                                                                     recommended.insecticide.dose = 1,
                                                                                     threshold.generations = c(mixture.parameter.df$threshold.gens.1[i], mixture.parameter.df$threshold.gens.2[i]),#no decay, so this value does not matter
                                                                                     base.efficacy.decay.rate = c(mixture.parameter.df$base.decay.1[i], mixture.parameter.df$base.decay.2[i]),
                                                                                     rapid.decay.rate = 0.08,
                                                                                     population.suppression = FALSE,
                                                                                     min.cross.selection = 0,
                                                                                     max.cross.selection = 0,
                                                                                     deployment.type = "mixtures",
                                                                                     mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 200, number.of.insecticides = 2)


  B.novel = A%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 1)

  B.pyrethroid = A%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 2)

  novel.mix.peak[i] = max(B.novel$resistance.intensity)
  novel.mix.mean[i] = mean(B.novel$resistance.intensity)
  pyrethroid.mix.peak[i] =  max(B.pyrethroid$resistance.intensity)
  pyrethroid.mix.mean[i] =  mean(B.pyrethroid$resistance.intensity)

  if(i %% 50 == 0){print(i)}

}

mixture.parameter.df$novel.mix.peak = novel.mix.peak
mixture.parameter.df$novel.mix.mean = novel.mix.mean
mixture.parameter.df$pyrethroid.mix.peak = pyrethroid.mix.peak
mixture.parameter.df$pyrethroid.mix.mean = pyrethroid.mix.mean



write.csv(mixture.parameter.df, ".//mixture.parameter.decay.SDscaled.csv")



pyrethroid.parameter.df = rbind(solo.parameter.df, solo.parameter.df, solo.parameter.df, solo.parameter.df)
pyrethroid.parameter.df$start.resistance.2.solo = c(rep(0, 1800), rep(100, 1800), rep(900, 1800), rep(3600, 1800))

pyrethroid.solo.peak = c()
pyrethroid.solo.mean = c()

for(i in 1:7200){
  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 1,
                                                                            female.fitness.cost = 0,
                                                                            male.fitness.cost = 0,
                                                                            female.insecticide.exposure = pyrethroid.parameter.df$female.exposure[i],
                                                                            male.insecticide.exposure = pyrethroid.parameter.df$male.exposure[i],
                                                                            heritability = pyrethroid.parameter.df$heritability[i],
                                                                            dispersal.rate = pyrethroid.parameter.df$dispersal[i],
                                                                            intervention.coverage = pyrethroid.parameter.df$coverage[i],
                                                                            standard.deviation = NA,
                                                                            sd.scaled = TRUE,
                                                                            z.sd.coefficient = 0.4,
                                                                            z.sd.intercept = 18,
                                                                            vector.length = 1000,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            regression.coefficient = 0.48,
                                                                            regression.intercept = 0.15,
                                                                            maximum.generations = 200,
                                                                            irm.strategy = "sequence",
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            withdrawal.threshold.value = 1,
                                                                            return.threshold.value = 0.08,
                                                                            deployment.frequency = 30, #minimum deployment frequency
                                                                            maximum.resistance.value = 90000,
                                                                            starting.refugia.resistance.score = pyrethroid.parameter.df$start.resistance.2.solo[i],
                                                                            starting.intervention.resistance.score = pyrethroid.parameter.df$start.resistance.2.solo[i],
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = pyrethroid.parameter.df$threshold.gens[i],#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = pyrethroid.parameter.df$base.decay[i],
                                                                            rapid.decay.rate = 0.08,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA),
                               maximum.generations = 200, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  pyrethroid.solo.peak[i] = max(B$resistance.score)
  pyrethroid.solo.mean[i] = mean(B$resistance.score)

  if(i %% 50 == 0){print(i)}
}


pyrethroid.parameter.df$pyrethroid.solo.peak = pyrethroid.solo.peak
pyrethroid.parameter.df$pyrethroid.solo.mean = pyrethroid.solo.mean



# write.csv(pyrethroid.parameter.df, ".//pyrethroidsolo.parameter.decay.SDscaled.csv")



library(ggplot2)
library(dplyr)
solo.novel.df = read.csv(".//solo.parameter.decay.SDscaled.csv")
solo.pyrethroid.df = read.csv(".//pyrethroidsolo.parameter.decay.SDscaled.csv")
mixture.df = read.csv(".//mixture.parameter.decay.SDscaled.csv")

#Remove colomn X ALL

mixture.df = mixture.df%>%
  dplyr::select(-"X")

#solo.novel :
  # base.decay --> base.decay.1
  # threshold.gens -- > threshold.gens.1

solo.novel.df = solo.novel.df%>%
  dplyr::select(-"X")%>%
  dplyr::rename(threshold.gens.1 = "threshold.gens")%>%
  dplyr::rename(base.decay.1 = "base.decay")

#solo.pyrethroid :
  #base.decay --> base.decay.2
  #threshold.gens --> threshold.gens.2
  #start.resistance.2.solo --> start.resistance.2

solo.pyrethroid.df = solo.pyrethroid.df%>%
  dplyr::select(-"X", -"novel.solo.mean", -"novel.solo.peak")%>%
  dplyr::rename(threshold.gens.2 = "threshold.gens")%>%
  dplyr::rename(base.decay.2 = "base.decay")%>%
  dplyr::rename(start.resistance.2 = "start.resistance.2.solo")


#match the 3 comparisons together:::
join.1 = inner_join(solo.pyrethroid.df, solo.novel.df)
truncation.scaled.df = inner_join(join.1, mixture.df)


truncation.scaled.df$change.novel.mean = truncation.scaled.df$novel.solo.mean - truncation.scaled.df$novel.mix.mean
truncation.scaled.df$change.novel.peak = truncation.scaled.df$novel.solo.peak - truncation.scaled.df$novel.mix.peak
truncation.scaled.df$change.pyrethroid.peak = truncation.scaled.df$pyrethroid.solo.peak - truncation.scaled.df$pyrethroid.mix.peak
truncation.scaled.df$change.pyrethroid.mean = truncation.scaled.df$pyrethroid.solo.mean - truncation.scaled.df$pyrethroid.mix.mean

truncation.scaled.df$start.biossay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$start.resistance.2,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)*100

truncation.scaled.df$novel.solo.mean.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.solo.mean,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)

truncation.scaled.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.solo.peak,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)

truncation.scaled.df$pyrethroid.solo.mean.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$pyrethroid.solo.mean,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)

truncation.scaled.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$pyrethroid.solo.peak,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)



truncation.scaled.df$novel.mix.mean.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.mix.mean,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)

truncation.scaled.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.mix.peak,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)

truncation.scaled.df$pyrethroid.mix.mean.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                   trait.mean = truncation.scaled.df$pyrethroid.mix.mean,
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   michaelis.menten.slope = 1)

truncation.scaled.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                   trait.mean = truncation.scaled.df$pyrethroid.mix.peak,
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   michaelis.menten.slope = 1)




truncation.scaled.df$change.novel.mean.bioassay = truncation.scaled.df$novel.solo.mean.bioassay - truncation.scaled.df$novel.mix.mean.bioassay
truncation.scaled.df$change.novel.peak.bioassay = truncation.scaled.df$novel.solo.peak.bioassay - truncation.scaled.df$novel.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.peak.bioassay = truncation.scaled.df$pyrethroid.solo.peak.bioassay - truncation.scaled.df$pyrethroid.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.mean.bioassay = truncation.scaled.df$pyrethroid.solo.mean.bioassay - truncation.scaled.df$pyrethroid.mix.mean.bioassay


truncation.scaled.df$dosing.strategy = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                truncation.scaled.df$dose.2 == 1,
                                              yes = "FD_FD",
                                              no = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                            truncation.scaled.df$dose.2 == 0.5,
                                                          yes = "FD_HD",
                                                          no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                        truncation.scaled.df$dose.2 == 1,
                                                                      yes = "HD_FD",
                                                                      no = "HD_HD")))





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



truncation.scaled.df$threshold.time = factor(ifelse(truncation.scaled.df$threshold.gens.1 == 10 &
                                           truncation.scaled.df$threshold.gens.2 == 20,
                                         yes = "much earlier",
                                         no = ifelse(truncation.scaled.df$threshold.gens.1 == 10 &
                                                       truncation.scaled.df$threshold.gens.2 == 15,
                                                     yes = "earlier",
                                                     no = ifelse(truncation.scaled.df$threshold.gens.1 == 15 &
                                                                   truncation.scaled.df$threshold.gens.2 == 20,
                                                                 yes = "earlier",
                                                                 no = ifelse(truncation.scaled.df$threshold.gens.1 == 20 &
                                                                               truncation.scaled.df$threshold.gens.2 == 10,
                                                                             yes = "much later",
                                                                             no = ifelse(truncation.scaled.df$threshold.gens.1 == 15 &
                                                                                           truncation.scaled.df$threshold.gens.2 == 10,
                                                                                         yes = "later",
                                                                                         no = ifelse(truncation.scaled.df$threshold.gens.1 == 20 &
                                                                                                       truncation.scaled.df$threshold.gens.2 == 15,
                                                                                                     yes = "later",
                                                                                                     no = ifelse(truncation.scaled.df$threshold.gens.1 == truncation.scaled.df$threshold.gens.2,
                                                                                                                 yes = "same",
                                                                                                                 no = NA))))))), levels = c("much later", "later", "same", "earlier", "much earlier"))


sum(is.na(truncation.scaled.df$threshold.time))

range(truncation.scaled.df$change.novel.mean.bioassay)*100
range(truncation.scaled.df$change.novel.peak.bioassay)
range(truncation.scaled.df$change.pyrethroid.peak.bioassay)*100
range(truncation.scaled.df$change.pyrethroid.mean.bioassay)*100


truncation.scaled.df.weird.novel = truncation.scaled.df%>%
  dplyr::filter(change.novel.peak.bioassay < 0)

truncation.scaled.df.weird.pyrethroid = truncation.scaled.df%>%
  dplyr::filter(change.pyrethroid.peak.bioassay < 0)


ggplot(truncation.scaled.df, aes(x=change.novel.peak.bioassay*100,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  facet_grid(start.biossay.pyrethroid~.)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(truncation.scaled.df, aes(x=change.pyrethroid.peak.bioassay*100,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 0.1)+
  facet_grid(start.resistance.2~.)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

#Importance of Resistance
ggplot(truncation.scaled.df, aes(x=change.novel.peak.bioassay*100,
                                 fill = start.biossay.pyrethroid))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_viridis_c()+
  facet_grid(start.biossay.pyrethroid ~ .)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(truncation.scaled.df, aes(x=change.pyrethroid.peak.bioassay*100,
                                 fill = start.biossay.pyrethroid))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_viridis_c()+
  facet_grid(start.biossay.pyrethroid ~ .)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


#Importance of Dosing
ggplot(truncation.scaled.df, aes(x=change.novel.peak.bioassay*100,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 0.1)+
  facet_grid(dosing.strategy ~ .)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


ggplot(truncation.scaled.df, aes(x=change.pyrethroid.peak.bioassay*100,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 0.1)+
  facet_grid(dosing.strategy ~ .)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

#Importance of Decay
ggplot(truncation.scaled.df, aes(x=change.novel.peak.bioassay*100))+
  geom_histogram(binwidth = 0.1, colour = "#386cb0")+
  facet_grid(threshold.time ~ decay.rate)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(truncation.scaled.df, aes(x=change.pyrethroid.peak.bioassay*100))+
  geom_histogram(binwidth = 0.1, colour = "#386cb0")+
  facet_grid(threshold.time ~ decay.rate)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")



#Of course there will be interactions between all these aspects.

library(ggh4x)

truncation.scaled.df.80 = truncation.scaled.df%>%
  dplyr::filter(start.biossay.pyrethroid > 70)

ggplot(truncation.scaled.df.80, aes(x=change.novel.peak.bioassay*100,
                                       fill = dosing.strategy))+
  geom_histogram(binwidth = 0.1)+
  facet_nested(start.biossay.pyrethroid ~ threshold.time ~ decay.rate)+
  xlab("Change in Peak Biossay Survival(%)")+
  ylab("Frequency")+
  ggtitle("Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")





##Mixtures seem to be best regardless except...........

ggplot(truncation.scaled.df.weird.novel, aes(x=threshold.time,
                                       y=decay.rate,
                                       colour = dosing.strategy))+
  geom_tile()+
  xlab("Threshold Generation Compared to Pyrethroid")+
  ylab("Decay Rate Compared to Pyrethroid")+
  geom_jitter()+
  theme_bw()


colnames(truncation.scaled.df)

ggplot(truncation.scaled.df, aes(x=dosing.strategy,
                                 y=novel.mix.peak.bioassay,
                                 colour = as.factor(start.biossay.pyrethroid)))+
  geom_boxplot()




#calculate changes in rates of evolution:::
truncation.scaled.df$novel.solo.rate = truncation.scaled.df$novel.solo.peak.bioassay/200
truncation.scaled.df$pyrethroid.solo.rate = (truncation.scaled.df$pyrethroid.solo.peak.bioassay - (truncation.scaled.df$start.biossay.pyrethroid/100))/200
truncation.scaled.df$novel.mix.rate = truncation.scaled.df$novel.mix.peak.bioassay/200
truncation.scaled.df$pyrethroid.mix.rate = (truncation.scaled.df$pyrethroid.mix.peak.bioassay - (truncation.scaled.df$start.biossay.pyrethroid/100))/200


truncation.scaled.df$rate.change.novel.percent = ((truncation.scaled.df$novel.mix.rate - truncation.scaled.df$novel.solo.rate)/truncation.scaled.df$novel.solo.rate) * 100
truncation.scaled.df$rate.change.pyrethroid.percent = ((truncation.scaled.df$pyrethroid.mix.rate - truncation.scaled.df$pyrethroid.solo.rate)/truncation.scaled.df$pyrethroid.solo.rate) * 100

library(ggridges)

novel.plot = ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, colour = "black")+
  facet_grid(start.biossay.pyrethroid ~ .)+
  ylab("Frequency")+
  xlab("Change in the Rate of Evolution (%)")+
  ggtitle("Novel Insecticide")+
  theme_classic()+
  theme(text = element_text(size=20),
        legend.position = "none")


pyrethroid.plot = ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, colour = "black")+
  facet_grid(start.biossay.pyrethroid ~ .)+
  ylab("Frequency")+
  xlab("Change in the Rate of Evolution (%)")+
  ggtitle("Pyrethroid Insecticide")+
  theme_classic()+
  theme(text = element_text(size=20),
        legend.position = "none")

fig.legend = cowplot::get_legend(ggplot(truncation.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                                       fill = dosing.strategy))+
                      geom_histogram(binwidth = 1, colour = "black")+
                      geom_vline(xintercept = 0, colour = "black")+
                      theme(text = element_text(size=20),
                            legend.position = "bottom"))

library(patchwork)
plot.layout = "
AAABBB
AAABBB
AAABBB
#CCCC#"

novel.plot + pyrethroid.plot + fig.legend + plot_layout(design = plot.layout)

ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = dosing.strategy))+
  geom_histogram(colour = "black")+
  facet_grid(decay.rate ~ threshold.time)+
  xlab("Change in the Rate of Evolution (%) - Novel")+
  theme_classic()




ggplot(truncation.scaled.df, aes(x=rate.change.novel.percent,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth=5, colour = "black")+
  geom_vline(xintercept = 0, colour = "black",
             size = 1, linetype = "dashed")+
  facet_grid(start.biossay.pyrethroid ~ dosing.strategy)+
  xlab("Change in the Rate of Evolution (%) Compared to Full Dose Novel Only")+
  ylab("Frequency")+
  theme_classic()+
  theme(legend.position = "none")




A = ggplot(subset(truncation.scaled.df, dosing.strategy == "FD_FD"), aes(x=rate.change.novel.percent,
                                 fill = dosing.strategy))+
  geom_histogram(binwidth=2, colour = "black")+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "HD_HD"), aes(x=rate.change.novel.percent,
                                                                               fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "FD_HD"), aes(x=rate.change.novel.percent,
                                                                                      fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "HD_FD"), aes(x=rate.change.novel.percent,
                                                                                      fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  scale_fill_manual(values = c("#1b9e77",
                               "#e7298a",
                               "#d95f02",
                               "#7570b3"
                               ))+
  geom_vline(xintercept = 0, colour = "black",
             size = 1, linetype = "dashed")+
  facet_grid(start.biossay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ylim(0, 2500)+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size=20))

B = ggplot(subset(truncation.scaled.df, dosing.strategy == "FD_FD"), aes(x=rate.change.pyrethroid.percent,
                                                                     fill = dosing.strategy))+
  geom_histogram(binwidth=2, colour = "black")+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "HD_HD"), aes(x=rate.change.pyrethroid.percent,
                                                                               fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "FD_HD"), aes(x=rate.change.pyrethroid.percent,
                                                                                      fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  geom_histogram(data = subset(truncation.scaled.df, dosing.strategy == "HD_FD"), aes(x=rate.change.pyrethroid.percent,
                                                                                      fill = dosing.strategy),
                 colour = "black", binwidth = 2)+
  scale_fill_manual(values = c("#1b9e77",
                              "#e7298a",
                              "#d95f02",
                              "#7570b3"

                              ))+
  geom_vline(xintercept = 0, colour = "black",
             size = 1, linetype = "dashed")+
  facet_grid(start.biossay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ylim(0, 2500)+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size=20))



A+ B











#Try and quantify these rates:

truncation.scaled.df$dosing.resistance = paste(truncation.scaled.df$dosing.strategy, truncation.scaled.df$start.biossay.pyrethroid)

df.novel = truncation.scaled.df%>%
  dplyr::group_by(dosing.strategy, as.character(start.biossay.pyrethroid))%>%
  summarise(mean(rate.change.novel.percent))

df.novel.1 = truncation.scaled.df%>%
  dplyr::group_by(dosing.strategy, as.character(start.biossay.pyrethroid))%>%
  summarise(sd(rate.change.novel.percent))

df.novel$mean.change = df.novel$`mean(rate.change.novel.percent)`
df.novel$st.dev = df.novel.1$`sd(rate.change.novel.percent)`
df.novel$start.bioassay = df.novel$`as.character(start.biossay.pyrethroid)`


df.pyrethroid = truncation.scaled.df%>%
  dplyr::group_by(dosing.strategy, as.character(start.biossay.pyrethroid))%>%
  summarise(mean(rate.change.pyrethroid.percent))

df.pyrethroid.1 = truncation.scaled.df%>%
  dplyr::group_by(dosing.strategy, as.character(start.biossay.pyrethroid))%>%
  summarise(sd(rate.change.pyrethroid.percent))

df.pyrethroid$mean.change = df.pyrethroid$`mean(rate.change.pyrethroid.percent)`
df.pyrethroid$st.dev = df.pyrethroid.1$`sd(rate.change.pyrethroid.percent)`
df.pyrethroid$start.bioassay = df.pyrethroid$`as.character(start.biossay.pyrethroid)`


ggplot(df.novel, aes(y=mean.change,
               x=as.numeric(start.bioassay),
                colour = dosing.strategy))+
  xlab("Initial Pyrethroid Resistance, Bioassay Survival (%)")+
  ylab("Mean Change (%)in the Rate of Evolution - Novel Insecticide")+
  geom_line(size = 3)+
  ylim(-15, -75)+
  theme_bw() +
ggplot(df.pyrethroid, aes(y=mean.change,
                     x=as.numeric(start.bioassay),
                     colour = dosing.strategy))+
  geom_line(size = 3)+
  ylim(-15, -75)+
  xlab("Initial Pyrethroid Resistance, Bioassay Survival (%)")+
  ylab("Mean Change (%) in the Rate of Evolution - Pyrethroid")+
  theme_bw()


truncation.lm = lm(rate.change.novel.percent ~
                     dosing.strategy,
                   data = truncation.scaled.df)

summary(truncation.lm)
ggplot(truncation.scaled.df, aes(x=as.factor(start.biossay.pyrethroid),
                                 y=rate.change.novel.percent,
                                 colour = dosing.strategy))+
geom_boxplot()





colnames(truncation.scaled.df)

ggplot(truncation.scaled.df, aes(x=change.novel.peak.bioassay*100))+
  geom_histogram()









