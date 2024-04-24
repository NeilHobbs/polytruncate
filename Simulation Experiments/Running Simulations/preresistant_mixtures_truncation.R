#Comparing sequences, rotations and mixtures under the smooth selection paradigm.

library(devtools)
load_all()

parameter.space.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")

#need to change the sign of the fitness cost parameters:
parameter.space.df$Female.Fitness.Cost = parameter.space.df$Female.Fitness.Cost  * -1
parameter.space.df$Male.Fitness.Cost = parameter.space.df$Male.Fitness.Cost  * -1




solo.list.novel = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 1,
                                                                            female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                            male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                            female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                            male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                            heritability = parameter.space.df$Heritability[v],
                                                                            dispersal.rate = parameter.space.df$Dispersal[v],
                                                                            intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                            standard.deviation = 20,
                                                                            vector.length = 1000,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            regression.coefficient = 0.48,
                                                                            regression.intercept = 0.15,
                                                                            maximum.generations = 500,
                                                                            irm.strategy = "sequence",
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            withdrawal.threshold.value = 0.1,
                                                                            return.threshold.value = 0.08,
                                                                            deployment.frequency = 10, #minimum deployment frequency
                                                                            maximum.resistance.value = 25000,
                                                                            starting.refugia.resistance.score = 0,
                                                                            starting.intervention.resistance.score = 0,
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = 5,#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = 0,
                                                                            rapid.decay.rate = 0,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  solo.list.novel[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, solo.list.novel)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//solo.novel.truncation.csv")



#Now do as mixture with pre-resistance:::
start.resistance.old = c(rep(0, 5000),
                         rep(18, 5000),
                         rep(47, 5000),
                         rep(100, 5000),
                         rep(225, 5000),
                         rep(900, 5000),
                         rep(3600, 5000),
                         rep(8100, 5000))

parameter.space.df = rbind(parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df)

parameter.space.df$start.resistance.old = start.resistance.old





solo.list.resistant = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                        exposure.scaling.factor = 1,
                                                                        female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                        male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                        female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                        male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                        heritability = parameter.space.df$Heritability[v],
                                                                        dispersal.rate = parameter.space.df$Dispersal[v],
                                                                        intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                        standard.deviation = 20,
                                                                        vector.length = 1000,
                                                                        maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1,
                                                                        regression.coefficient = 0.48,
                                                                        regression.intercept = 0.15,
                                                                        maximum.generations = 500,
                                                                        irm.strategy = "sequence",
                                                                        half.population.bioassay.survival.resistance = 900,
                                                                        withdrawal.threshold.value = 1,
                                                                        return.threshold.value = 0.08,
                                                                        deployment.frequency = 10, #minimum deployment frequency
                                                                        maximum.resistance.value = 25000,
                                                                        starting.refugia.resistance.score = 0,
                                                                        starting.intervention.resistance.score = 0,
                                                                        applied.insecticide.dose = 1,
                                                                        recommended.insecticide.dose = 1,
                                                                        threshold.generations = 5,#no decay, so this value does not matter
                                                                        base.efficacy.decay.rate = 0,
                                                                        rapid.decay.rate = 0,
                                                                        population.suppression = FALSE,
                                                                        min.cross.selection = 0,
                                                                        max.cross.selection = 0,
                                                                        deployment.type = "singles",
                                                                        mixture.strategy = NA),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)
  mean.resistance = mean(B$resistance.score)
  peak.resistance = max(B$resistance.score)

  solo.list.resistant[[v]] = data.frame(simulation.duration, mean.resistance, peak.resistance)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, solo.list.resistant)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//solo.resistant.truncation.csv")


mixture.list = list() #need to adapt code to make decision only on insecticide 1.

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                 exposure.scaling.factor = 1,
                                                                                 female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                                 male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                                 female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                 male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                 heritability = parameter.space.df$Heritability[v],
                                                                                 dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                 intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                 standard.deviation = 20,
                                                                                 vector.length = 1000,
                                                                                 maximum.bioassay.survival.proportion = 1,
                                                                                 michaelis.menten.slope = 1,
                                                                                 regression.coefficient = 0.48,
                                                                                 regression.intercept = 0.15,
                                                                                 maximum.generations = 500,
                                                                                 irm.strategy = "insecticide.1",
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 withdrawal.threshold.value = 0.1,
                                                                                 return.threshold.value = 0.08,
                                                                                 deployment.frequency = 10, #minimum deployment frequency
                                                                                 maximum.resistance.value = 25000,
                                                                                 starting.refugia.resistance.score = c(0, start.resistance.old[v]),
                                                                                 starting.intervention.resistance.score = c(0, start.resistance.old[v]),
                                                                                 applied.insecticide.dose = 1,
                                                                                 recommended.insecticide.dose = 1,
                                                                                 threshold.generations = 5,#no decay, so this value does not matter
                                                                                 base.efficacy.decay.rate = 0,
                                                                                 rapid.decay.rate = 0,
                                                                                 population.suppression = FALSE,
                                                                                 min.cross.selection = 0,
                                                                                 max.cross.selection = 0,
                                                                                 deployment.type = "mixtures",
                                                                                 mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)

  #insecticide.1
  B.1 = B%>%
    dplyr::filter(insecticide.tracked == 1)
  mean.insecticide.1 = mean(B.1$resistance.intensity)
  peak.insecticide.1 = max(B.1$resistance.intensity)

  #insecticide.2
  B.2 = B%>%
    dplyr::filter(insecticide.tracked == 2)
  mean.insecticide.2 = mean(B.2$resistance.intensity)
  peak.insecticide.2 = max(B.2$resistance.intensity)



  mixture.list[[v]] = data.frame(simulation.duration, mean.insecticide.1, peak.insecticide.1,
                                 mean.insecticide.2, peak.insecticide.2)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, mixture.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//mixture.preresistance.truncation.csv")



#read in data sets

solo.insecticide.novel = read.csv("solo.novel.truncation.csv")
mixture.insecticide  = read.csv("mixture.preresistance.truncation.csv")

solo.insecticide.novel.full = rbind(solo.insecticide.novel, solo.insecticide.novel, solo.insecticide.novel, solo.insecticide.novel,
                                    solo.insecticide.novel, solo.insecticide.novel, solo.insecticide.novel, solo.insecticide.novel)





mixture.insecticide$diff = mixture.insecticide$simulation.duration - solo.insecticide.novel.full$sim.duration

mixture.insecticide$prop.diff = (mixture.insecticide$simulation.duration - solo.insecticide.novel.full$sim.duration)/mixture.insecticide$simulation.duration



hist(mixture.insecticide$mean.insecticide.1)
hist(solo.insecticide.novel.full$mean.resistance)

head(solo.insecticide.novel.full)

library(ggplot2)

mixture.insecticide$category = ifelse(mixture.insecticide$diff > 0,
                                       yes = "favours mixtures",
                                       no = "no change")

mixture.insecticide$start.bioassay.old = c(rep(0, 5000), rep(2, 5000),
                                             rep(5, 5000), rep(10, 5000), rep(20, 5000),
                                             rep(50, 5000), rep(80, 5000), rep(90, 5000))

ggplot(mixture.insecticide, aes(x=prop.diff*100,
                                fill = category))+
  geom_histogram(binwidth = 3,
                 colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#276419", "#999999"))+
  facet_wrap(~start.bioassay.old)+
  xlab("Percentage Change in Operational Lifespan")+
  theme_classic()


min(mixture.insecticide$diff)




#Run with both insecticides at half efficacy ("half dose")
mixture.list.half = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                     exposure.scaling.factor = 1,
                                                                                     female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                                     male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                                     female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                     male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                     heritability = parameter.space.df$Heritability[v],
                                                                                     dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                     intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                     standard.deviation = 20,
                                                                                     vector.length = 1000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     regression.coefficient = 0.48,
                                                                                     regression.intercept = 0.15,
                                                                                     maximum.generations = 500,
                                                                                     irm.strategy = "insecticide.1",
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     withdrawal.threshold.value = 0.1,
                                                                                     return.threshold.value = 0.08,
                                                                                     deployment.frequency = 10, #minimum deployment frequency
                                                                                     maximum.resistance.value = 25000,
                                                                                     starting.refugia.resistance.score = c(0, start.resistance.old[v]),
                                                                                     starting.intervention.resistance.score = c(0, start.resistance.old[v]),
                                                                                     applied.insecticide.dose = 0.5,
                                                                                     recommended.insecticide.dose = 1,
                                                                                     threshold.generations = 5,#no decay, so this value does not matter
                                                                                     base.efficacy.decay.rate = 0,
                                                                                     rapid.decay.rate = 0,
                                                                                     population.suppression = FALSE,
                                                                                     min.cross.selection = 0,
                                                                                     max.cross.selection = 0,
                                                                                     deployment.type = "mixtures",
                                                                                     mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)

  #insecticide.1
  B.1 = B%>%
    dplyr::filter(insecticide.tracked == 1)
  mean.insecticide.1 = mean(B.1$resistance.intensity)
  peak.insecticide.1 = max(B.1$resistance.intensity)

  #insecticide.2
  B.2 = B%>%
    dplyr::filter(insecticide.tracked == 2)
  mean.insecticide.2 = mean(B.2$resistance.intensity)
  peak.insecticide.2 = max(B.2$resistance.intensity)



  mixture.list.half[[v]] = data.frame(simulation.duration, mean.insecticide.1, peak.insecticide.1,
                                 mean.insecticide.2, peak.insecticide.2)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, mixture.list.half)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//mixture.preresistance.truncation.half.csv")


############
mixture.preresistance.truncation.half = read.csv("mixture.preresistance.truncation.half.csv")


mixture.preresistance.truncation.half$half.diff = mixture.preresistance.truncation.half$simulation.duration - solo.insecticide.novel.full$sim.duration

mixture.preresistance.truncation.half$prop.half.diff = (mixture.preresistance.truncation.half$simulation.duration - solo.insecticide.novel.full$sim.duration)/mixture.preresistance.truncation.half$simulation.duration


mixture.preresistance.truncation.half$category = ifelse(mixture.preresistance.truncation.half$half.diff > 0,
                                      yes = "favours mixtures",
                                      no = ifelse(mixture.preresistance.truncation.half$half.diff < 0,
                                                  yes = "favours solo",
                                                  no = "no change"))

mixture.preresistance.truncation.half$start.bioassay.old = c(rep(0, 5000), rep(2, 5000),
                                           rep(5, 5000), rep(10, 5000), rep(20, 5000),
                                           rep(50, 5000), rep(80, 5000), rep(90, 5000))

ggplot(mixture.preresistance.truncation.half, aes(x=prop.half.diff*100,
                                fill = category))+
  geom_histogram(binwidth = 3,
                 colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#ffff33", "#999999"))+
  facet_wrap(~start.bioassay.old)+
  xlab("Percentage Change in Operational Lifespan")+
  theme_classic()

ggplot(mixture.preresistance.truncation.half, aes(x=Female.Insecticide.Exposure))+
  geom_histogram(colour = "black",
                 fill = "#bebada",
                 bins = 30)+
  facet_grid(start.bioassay.old~category)+
  theme_classic()

mixture.preresistance.truncation.half.solo = mixture.preresistance.truncation.half%>%
  dplyr::filter(category == "favours solo")


ggplot(mixture.preresistance.truncation.half, aes(x=Female.Insecticide.Exposure*Male.Insecticide.Exposure))+
  geom_histogram(colour = "black",
                 fill = "#bebada",
                 bins = 30)+
  xlab("Male Encounter Probability")+
  facet_grid(start.bioassay.old~category)+
  theme_classic()


mixture.preresistance.truncation.half.high = mixture.preresistance.truncation.half%>%
  dplyr::filter(start.bioassay.old >= 80)%>%
  dplyr::filter(category != "no change")

ggplot(mixture.preresistance.truncation.half.high, aes(x=Female.Insecticide.Exposure,
                                                       y=Male.Insecticide.Exposure,
                                                       colour = category))+
  geom_point()+
  theme_bw()


ggplot(mixture.preresistance.truncation.half.high, aes(x=Intervention.Coverage,
                                                       y=Heritability,
                                                       colour = category))+
  geom_point()+
  theme_bw()


mixture.insecticide$change.old.half = mixture.preresistance.truncation.half$peak.insecticide.2 - mixture.preresistance.truncation.half$start.resistance.old
mixture.insecticide$change.old.full = mixture.insecticide$peak.insecticide.2 - mixture.insecticide$start.resistance.old
mixture.insecticide$change.diff.old = mixture.insecticide$change.old.full - mixture.insecticide$change.old.half

mixture.insecticide$diff.category = ifelse(mixture.insecticide$change.diff.old > 5,
                                           yes = "favours half dose",
                                           no = ifelse(mixture.insecticide$change.diff.old < -5,
                                                       yes = "favours full dose",
                                                       no = "no preference"))

mixture.insecticide$full.half.diff = mixture.insecticide$simulation.duration - mixture.preresistance.truncation.half$simulation.duration

ggplot(mixture.insecticide, aes(x=full.half.diff/10))+
  geom_histogram(binwidth = 1,
                 colour = "grey",
                 fill = "green")+
  xlab("Difference in Operational Lifespan (Years)")+
  theme_classic()+
  facet_wrap(~start.bioassay.old)




#get a single parameter set to plot and have a look at::

mixture.preresistance.truncation.half.solo[1, ]

half.mix.sim.example = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                                      exposure.scaling.factor = 1,
                                                                                                      female.fitness.cost = -0.2668767,
                                                                                                      male.fitness.cost = -0.5403798,
                                                                                                      female.insecticide.exposure = 0.4028153,
                                                                                                      male.insecticide.exposure = 0.6959418,
                                                                                                      heritability = 0.2165601,
                                                                                                      dispersal.rate = 0.4837409,
                                                                                                      intervention.coverage = 0.7135403,
                                                                                                      standard.deviation = 20,
                                                                                                      vector.length = 1000,
                                                                                                      maximum.bioassay.survival.proportion = 1,
                                                                                                      michaelis.menten.slope = 1,
                                                                                                      regression.coefficient = 0.48,
                                                                                                      regression.intercept = 0.15,
                                                                                                      maximum.generations = 500,
                                                                                                      irm.strategy = "insecticide.1",
                                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                                      withdrawal.threshold.value = 0.1,
                                                                                                      return.threshold.value = 0.08,
                                                                                                      deployment.frequency = 10, #minimum deployment frequency
                                                                                                      maximum.resistance.value = 25000,
                                                                                                      starting.refugia.resistance.score = c(0, 3600),
                                                                                                      starting.intervention.resistance.score = c(0, 3600),
                                                                                                      applied.insecticide.dose = 0.5,
                                                                                                      recommended.insecticide.dose = 1,
                                                                                                      threshold.generations = 5,#no decay, so this value does not matter
                                                                                                      base.efficacy.decay.rate = 0,
                                                                                                      rapid.decay.rate = 0,
                                                                                                      population.suppression = FALSE,
                                                                                                      min.cross.selection = 0,
                                                                                                      max.cross.selection = 0,
                                                                                                      deployment.type = "mixtures",
                                                                                                      mixture.strategy = "mix.sequential.discrete"),
                                                         maximum.generations = 500, number.of.insecticides = 2)



solo.full.sim.example = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                                                      exposure.scaling.factor = 1,
                                                                                                      female.fitness.cost = -0.2668767,
                                                                                                      male.fitness.cost = -0.5403798,
                                                                                                      female.insecticide.exposure = 0.4028153,
                                                                                                      male.insecticide.exposure = 0.6959418,
                                                                                                      heritability = 0.2165601,
                                                                                                      dispersal.rate = 0.4837409,
                                                                                                      intervention.coverage = 0.7135403,
                                                                                                      standard.deviation = 20,
                                                                                                      vector.length = 1000,
                                                                                                      maximum.bioassay.survival.proportion = 1,
                                                                                                      michaelis.menten.slope = 1,
                                                                                                      regression.coefficient = 0.48,
                                                                                                      regression.intercept = 0.15,
                                                                                                      maximum.generations = 500,
                                                                                                      irm.strategy = "sequence",
                                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                                      withdrawal.threshold.value = 0.1,
                                                                                                      return.threshold.value = 0.08,
                                                                                                      deployment.frequency = 10, #minimum deployment frequency
                                                                                                      maximum.resistance.value = 25000,
                                                                                                      starting.refugia.resistance.score = 0,
                                                                                                      starting.intervention.resistance.score = 0,
                                                                                                      applied.insecticide.dose = 1,
                                                                                                      recommended.insecticide.dose = 1,
                                                                                                      threshold.generations = 5,#no decay, so this value does not matter
                                                                                                      base.efficacy.decay.rate = 0,
                                                                                                      rapid.decay.rate = 0,
                                                                                                      population.suppression = FALSE,
                                                                                                      min.cross.selection = 0,
                                                                                                      max.cross.selection = 0,
                                                                                                      deployment.type = "singles",
                                                                                                      mixture.strategy = "mix.sequential.discrete"),
                                                         maximum.generations = 500, number.of.insecticides = 1)


solo.full.sim.example.intervention = solo.full.sim.example%>%
  dplyr::filter(site == "refugia")

half.mix.sim.example.intervention = half.mix.sim.example%>%
  dplyr::filter(site == "refugia")%>%
  dplyr::filter(insecticide.tracked == 1)


ggplot(half.mix.sim.example.intervention, aes(x=time.in.generations,
                                              y=resistance.intensity))+
  geom_line(colour = "red")+
  geom_line(data = solo.full.sim.example.intervention, aes(x=time.in.generations,
                                                    y=resistance.score),
            colour = "blue")+
  xlim(0, 10)+
  ylim(0, 4)










































