##How many solo insecticides is a mixture worth???

temp_function = function(female.insecticide.exposure,
                         male.insecticide.exposure,
                         heritability,
                         dispersal.rate,
                         intervention.coverage){

full.mix = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                                   exposure.scaling.factor = 1,
                                                                                                   female.fitness.cost = 0,
                                                                                                   male.fitness.cost = 0,
                                                                                                   male.insecticide.exposure = male.insecticide.exposure,
                                                                                                   female.insecticide.exposure = female.insecticide.exposure,
                                                                                                   heritability = heritability,
                                                                                                   dispersal.rate = dispersal.rate,
                                                                                                   intervention.coverage = intervention.coverage,
                                                                                                   standard.deviation = 20,
                                                                                                   vector.length = 250,
                                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                                   michaelis.menten.slope = 1,
                                                                                                   regression.coefficient = 0.48,
                                                                                                   regression.intercept = 0.15,
                                                                                                   maximum.generations = 1000,
                                                                                                   irm.strategy = "sequence",
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   withdrawal.threshold.value = 0.1,
                                                                                                   return.threshold.value = 0.08,
                                                                                                   deployment.frequency = 2, #minimum deployment frequency
                                                                                                   maximum.resistance.value = 25000,
                                                                                                   starting.refugia.resistance.score = 0,
                                                                                                   starting.intervention.resistance.score = 0,
                                                                                                   applied.insecticide.dose = 1,
                                                                                                   recommended.insecticide.dose = 1,
                                                                                                   threshold.generations = 10,#no decay, so this value does not matter
                                                                                                   base.efficacy.decay.rate = 0,
                                                                                                   rapid.decay.rate = 0,
                                                                                                   population.suppression = FALSE,
                                                                                                   min.cross.selection = 0,
                                                                                                   max.cross.selection = 0,
                                                                                                   deployment.type = "mixtures",
                                                                                                   mixture.strategy = "mix.sequential.discrete"),
                                                      maximum.generations = 1000, number.of.insecticides = 2)


half.mix.50 = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                                   exposure.scaling.factor = 1,
                                                                                                   female.fitness.cost = 0,
                                                                                                   male.fitness.cost = 0,
                                                                                                   male.insecticide.exposure = male.insecticide.exposure,
                                                                                                   female.insecticide.exposure = female.insecticide.exposure,
                                                                                                   heritability = heritability,
                                                                                                   dispersal.rate = dispersal.rate,
                                                                                                   intervention.coverage = intervention.coverage,
                                                                                                   standard.deviation = 20,
                                                                                                   vector.length = 250,
                                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                                   michaelis.menten.slope = 1,
                                                                                                   regression.coefficient = 0.48,
                                                                                                   regression.intercept = 0.15,
                                                                                                   maximum.generations = 1000,
                                                                                                   irm.strategy = "sequence",
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   withdrawal.threshold.value = 0.1,
                                                                                                   return.threshold.value = 0.08,
                                                                                                   deployment.frequency = 2, #minimum deployment frequency
                                                                                                   maximum.resistance.value = 25000,
                                                                                                   starting.refugia.resistance.score = 0,
                                                                                                   starting.intervention.resistance.score = 0,
                                                                                                   applied.insecticide.dose = 0.5,
                                                                                                   recommended.insecticide.dose = 1,
                                                                                                   threshold.generations = 10,#no decay, so this value does not matter
                                                                                                   base.efficacy.decay.rate = 0,
                                                                                                   rapid.decay.rate = 0,
                                                                                                   population.suppression = FALSE,
                                                                                                   min.cross.selection = 0,
                                                                                                   max.cross.selection = 0,
                                                                                                   deployment.type = "mixtures",
                                                                                                   mixture.strategy = "mix.sequential.discrete"),
                                                      maximum.generations = 1000, number.of.insecticides = 2)

half.mix.75 = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                             exposure.scaling.factor = 1,
                                                                                             female.fitness.cost = 0,
                                                                                             male.fitness.cost = 0,
                                                                                             male.insecticide.exposure = male.insecticide.exposure,
                                                                                             female.insecticide.exposure = female.insecticide.exposure,
                                                                                             heritability = heritability,
                                                                                             dispersal.rate = dispersal.rate,
                                                                                             intervention.coverage = intervention.coverage,
                                                                                             standard.deviation = 20,
                                                                                             vector.length = 250,
                                                                                             maximum.bioassay.survival.proportion = 1,
                                                                                             michaelis.menten.slope = 1,
                                                                                             regression.coefficient = 0.48,
                                                                                             regression.intercept = 0.15,
                                                                                             maximum.generations = 1000,
                                                                                             irm.strategy = "sequence",
                                                                                             half.population.bioassay.survival.resistance = 900,
                                                                                             withdrawal.threshold.value = 0.1,
                                                                                             return.threshold.value = 0.08,
                                                                                             deployment.frequency = 2, #minimum deployment frequency
                                                                                             maximum.resistance.value = 25000,
                                                                                             starting.refugia.resistance.score = 0,
                                                                                             starting.intervention.resistance.score = 0,
                                                                                             applied.insecticide.dose = 0.75,
                                                                                             recommended.insecticide.dose = 1,
                                                                                             threshold.generations = 10,#no decay, so this value does not matter
                                                                                             base.efficacy.decay.rate = 0,
                                                                                             rapid.decay.rate = 0,
                                                                                             population.suppression = FALSE,
                                                                                             min.cross.selection = 0,
                                                                                             max.cross.selection = 0,
                                                                                             deployment.type = "mixtures",
                                                                                             mixture.strategy = "mix.sequential.discrete"),
                                                maximum.generations = 1000, number.of.insecticides = 2)

sequence.1 = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                                   exposure.scaling.factor = 1,
                                                                                   female.fitness.cost = 0,
                                                                                   male.fitness.cost = 0,
                                                                                   male.insecticide.exposure = male.insecticide.exposure,
                                                                                   female.insecticide.exposure = female.insecticide.exposure,
                                                                                   heritability = heritability,
                                                                                   dispersal.rate = dispersal.rate,
                                                                                   intervention.coverage = intervention.coverage,
                                                                                   standard.deviation = 20,
                                                                                   vector.length = 250,
                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                   michaelis.menten.slope = 1,
                                                                                   regression.coefficient = 0.48,
                                                                                   regression.intercept = 0.15,
                                                                                   maximum.generations = 1000,
                                                                                   irm.strategy = "sequence",
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   withdrawal.threshold.value = 0.1,
                                                                                   return.threshold.value = 0.08,
                                                                                   deployment.frequency = 2, #minimum deployment frequency
                                                                                   maximum.resistance.value = 25000,
                                                                                   starting.refugia.resistance.score = 0,
                                                                                   starting.intervention.resistance.score = 0,
                                                                                   applied.insecticide.dose = 1,
                                                                                   recommended.insecticide.dose = 1,
                                                                                   threshold.generations = 10,#no decay, so this value does not matter
                                                                                   base.efficacy.decay.rate = 0,
                                                                                   rapid.decay.rate = 0,
                                                                                   population.suppression = FALSE,
                                                                                   min.cross.selection = 0,
                                                                                   max.cross.selection = 0,
                                                                                   deployment.type = "singles",
                                                                                   mixture.strategy = NA),
                                      maximum.generations = 1000, number.of.insecticides = 1)

# sequence.2 = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
#                                                                                    exposure.scaling.factor = 1,
#                                                                                    female.fitness.cost = 0,
#                                                                                    male.fitness.cost = 0,
#                                                                                    male.insecticide.exposure = male.insecticide.exposure,
#                                                                                    female.insecticide.exposure = female.insecticide.exposure,
#                                                                                    heritability = heritability,
#                                                                                    dispersal.rate = dispersal.rate,
#                                                                                    intervention.coverage = intervention.coverage,
#                                                                                    standard.deviation = 20,
#                                                                                    vector.length = 250,
#                                                                                    maximum.bioassay.survival.proportion = 1,
#                                                                                    michaelis.menten.slope = 1,
#                                                                                    regression.coefficient = 0.48,
#                                                                                    regression.intercept = 0.15,
#                                                                                    maximum.generations = 500,
#                                                                                    irm.strategy = "sequence",
#                                                                                    half.population.bioassay.survival.resistance = 900,
#                                                                                    withdrawal.threshold.value = 0.1,
#                                                                                    return.threshold.value = 0.08,
#                                                                                    deployment.frequency = 2, #minimum deployment frequency
#                                                                                    maximum.resistance.value = 25000,
#                                                                                    starting.refugia.resistance.score = 0,
#                                                                                    starting.intervention.resistance.score = 0,
#                                                                                    applied.insecticide.dose = 1,
#                                                                                    recommended.insecticide.dose = 1,
#                                                                                    threshold.generations = 10,#no decay, so this value does not matter
#                                                                                    base.efficacy.decay.rate = 0,
#                                                                                    rapid.decay.rate = 0,
#                                                                                    population.suppression = FALSE,
#                                                                                    min.cross.selection = 0,
#                                                                                    max.cross.selection = 0,
#                                                                                    deployment.type = "singles",
#                                                                                    mixture.strategy = NA),
#                                       maximum.generations = 500, number.of.insecticides = 2)
#
# sequence.3 = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 3,
#                                                                                    exposure.scaling.factor = 1,
#                                                                                    female.fitness.cost = 0,
#                                                                                    male.fitness.cost = 0,
#                                                                                    male.insecticide.exposure = male.insecticide.exposure,
#                                                                                    female.insecticide.exposure = female.insecticide.exposure,
#                                                                                    heritability = heritability,
#                                                                                    dispersal.rate = dispersal.rate,
#                                                                                    intervention.coverage = intervention.coverage,
#                                                                                    standard.deviation = 20,
#                                                                                    vector.length = 250,
#                                                                                    maximum.bioassay.survival.proportion = 1,
#                                                                                    michaelis.menten.slope = 1,
#                                                                                    regression.coefficient = 0.48,
#                                                                                    regression.intercept = 0.15,
#                                                                                    maximum.generations = 500,
#                                                                                    irm.strategy = "sequence",
#                                                                                    half.population.bioassay.survival.resistance = 900,
#                                                                                    withdrawal.threshold.value = 0.1,
#                                                                                    return.threshold.value = 0.08,
#                                                                                    deployment.frequency = 2, #minimum deployment frequency
#                                                                                    maximum.resistance.value = 25000,
#                                                                                    starting.refugia.resistance.score = 0,
#                                                                                    starting.intervention.resistance.score = 0,
#                                                                                    applied.insecticide.dose = 1,
#                                                                                    recommended.insecticide.dose = 1,
#                                                                                    threshold.generations = 10,#no decay, so this value does not matter
#                                                                                    base.efficacy.decay.rate = 0,
#                                                                                    rapid.decay.rate = 0,
#                                                                                    population.suppression = FALSE,
#                                                                                    min.cross.selection = 0,
#                                                                                    max.cross.selection = 0,
#                                                                                    deployment.type = "singles",
#                                                                                    mixture.strategy = NA),
#                                       maximum.generations = 500, number.of.insecticides = 3)
#
# sequence.4 = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 4,
#                                                                                    exposure.scaling.factor = 1,
#                                                                                    female.fitness.cost = 0,
#                                                                                    male.fitness.cost = 0,
#                                                                                    male.insecticide.exposure = male.insecticide.exposure,
#                                                                                    female.insecticide.exposure = female.insecticide.exposure,
#                                                                                    heritability = heritability,
#                                                                                    dispersal.rate = dispersal.rate,
#                                                                                    intervention.coverage = intervention.coverage,
#                                                                                    standard.deviation = 20,
#                                                                                    vector.length = 250,
#                                                                                    maximum.bioassay.survival.proportion = 1,
#                                                                                    michaelis.menten.slope = 1,
#                                                                                    regression.coefficient = 0.48,
#                                                                                    regression.intercept = 0.15,
#                                                                                    maximum.generations = 500,
#                                                                                    irm.strategy = "sequence",
#                                                                                    half.population.bioassay.survival.resistance = 900,
#                                                                                    withdrawal.threshold.value = 0.1,
#                                                                                    return.threshold.value = 0.08,
#                                                                                    deployment.frequency = 2, #minimum deployment frequency
#                                                                                    maximum.resistance.value = 25000,
#                                                                                    starting.refugia.resistance.score = 0,
#                                                                                    starting.intervention.resistance.score = 0,
#                                                                                    applied.insecticide.dose = 1,
#                                                                                    recommended.insecticide.dose = 1,
#                                                                                    threshold.generations = 10,#no decay, so this value does not matter
#                                                                                    base.efficacy.decay.rate = 0,
#                                                                                    rapid.decay.rate = 0,
#                                                                                    population.suppression = FALSE,
#                                                                                    min.cross.selection = 0,
#                                                                                    max.cross.selection = 0,
#                                                                                    deployment.type = "singles",
#                                                                                    mixture.strategy = NA),
#                                       maximum.generations = 500, number.of.insecticides = 4)
#
# sequence.5 = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 5,
#                                                                                    exposure.scaling.factor = 1,
#                                                                                    female.fitness.cost = 0,
#                                                                                    male.fitness.cost = 0,
#                                                                                    male.insecticide.exposure = male.insecticide.exposure,
#                                                                                    female.insecticide.exposure = female.insecticide.exposure,
#                                                                                    heritability = heritability,
#                                                                                    dispersal.rate = dispersal.rate,
#                                                                                    intervention.coverage = intervention.coverage,
#                                                                                    standard.deviation = 20,
#                                                                                    vector.length = 250,
#                                                                                    maximum.bioassay.survival.proportion = 1,
#                                                                                    michaelis.menten.slope = 1,
#                                                                                    regression.coefficient = 0.48,
#                                                                                    regression.intercept = 0.15,
#                                                                                    maximum.generations = 500,
#                                                                                    irm.strategy = "sequence",
#                                                                                    half.population.bioassay.survival.resistance = 900,
#                                                                                    withdrawal.threshold.value = 0.1,
#                                                                                    return.threshold.value = 0.08,
#                                                                                    deployment.frequency = 2, #minimum deployment frequency
#                                                                                    maximum.resistance.value = 25000,
#                                                                                    starting.refugia.resistance.score = 0,
#                                                                                    starting.intervention.resistance.score = 0,
#                                                                                    applied.insecticide.dose = 1,
#                                                                                    recommended.insecticide.dose = 1,
#                                                                                    threshold.generations = 10,#no decay, so this value does not matter
#                                                                                    base.efficacy.decay.rate = 0,
#                                                                                    rapid.decay.rate = 0,
#                                                                                    population.suppression = FALSE,
#                                                                                    min.cross.selection = 0,
#                                                                                    max.cross.selection = 0,
#                                                                                    deployment.type = "singles",
#                                                                                    mixture.strategy = NA),
#                                       maximum.generations = 500, number.of.insecticides = 5)


full.mix.lifespan = max(full.mix$time.in.generations)
half.mix.50.lifespan = max(half.mix.50$time.in.generations)
half.mix.75.lifespan = max(half.mix.75$time.in.generations)
sequence.1.lifespan =  max(sequence.1$time.in.generations)


lifespan = c(full.mix.lifespan, half.mix.50.lifespan, half.mix.75.lifespan, sequence.1.lifespan)
strategy = c("Full Mix", "Half Mix 50%", "Half Mix 75%", "1 solo")

df = data.frame(lifespan, strategy)

return(df)

}



male.insecticide.exposure = runif(10000, 0, 1)
female.insecticide.exposure = runif(10000, 0.4, 0.9)
heritability = runif(10000, 0.05, 0.3)
dispersal.rate = runif(10000, 0.1, 0.9)
intervention.coverage = runif(10000, 0.5, 0.9)

df.list = list()

for(i in 9635:10000){

  df = temp_function(male.insecticide.exposure[i],
                     female.insecticide.exposure[i],
                     heritability[i],
                     dispersal.rate[i],
                     intervention.coverage[i])
  df$X = rep(i, 4)

  df.list[[i]] = df

  print(i)
}


the.df = do.call(rbind, df.list)


the.df.full.mix = the.df%>%
  dplyr::filter(strategy == "Full Mix")

the.df.half.mix.50 = the.df%>%
  dplyr::filter(strategy == "Half Mix 50%")

the.df.half.mix.75 = the.df%>%
  dplyr::filter(strategy == "Half Mix 75%")

the.df.1.insecticide = the.df%>%
  dplyr::filter(strategy == "1 solo")



full.mix.value = the.df.full.mix$lifespan / the.df.1.insecticide$lifespan
half.mix.value.50 = the.df.half.mix.50$lifespan / the.df.1.insecticide$lifespan
half.mix.value.75 = the.df.half.mix.75$lifespan / the.df.1.insecticide$lifespan
solo.single.lifespan = the.df.1.insecticide$lifespan



value.df = data.frame(full.mix.value, half.mix.value.50, half.mix.value.75, solo.single.lifespan)




summary(value.df)

full.mix.value.plot = ggplot(value.df, aes(x=full.mix.value))+
  geom_histogram(colour = "black",
                 fill = "#084594",
                 binwidth = 0.1)+
  geom_vline(xintercept = 2, linetype = "dashed")+
  geom_vline(xintercept = mean(full.mix.value),
             colour = "#cb181d")+
  ggtitle("Full Dose Mixture")+
 xlab("Individual Insecticides")+
   ylab("Frequency")+
  xlim(0, 6)+
  ylim(0, 6500)+
  theme_classic()

half.mix.50.value.plot = ggplot(value.df, aes(x=half.mix.value.50))+
  geom_histogram(colour = "black",
                 fill = "#084594",
                 binwidth = 0.1)+
  geom_vline(xintercept = 2, linetype = "dashed")+
  geom_vline(xintercept = mean(half.mix.value.50),
             colour = "#cb181d")+
  ggtitle("Half Dose Mixture (50% Efficacy)")+
  xlab("Individual Insecticides")+
  ylab("Frequency")+
  xlim(0, 6)+
  ylim(0, 6500)+
  theme_classic()

half.mix.75.value.plot = ggplot(value.df, aes(x=half.mix.value.75))+
  geom_histogram(colour = "black",
                 fill = "#084594",
                 binwidth = 0.1)+
  geom_vline(xintercept = 2, linetype = "dashed")+
  geom_vline(xintercept = mean(half.mix.value.75),
             colour = "#cb181d")+
  ggtitle("Half Dose Mixture (75% Efficacy)")+
  xlab("Individual Insecticides")+
  ylab("Frequency")+
  xlim(0, 6)+
  ylim(0, 6500)+
  theme_classic()

#library(patchwork)

full.mix.value.plot + half.mix.50.value.plot + half.mix.75.value.plot +
  plot_annotation(title = "polytruncate",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

#######

ggplot(value.df, aes(x=full.mix.value))+
  geom_histogram(colour = "grey",
                 fill = "seagreen",
                 binwidth = 0.1)+
  geom_vline(xintercept = median(value.df$full.mix.value),
             linetype = "dashed",
             size = 2,
             colour = "coral")+
  geom_vline(xintercept = mean(value.df$full.mix.value),
             linetype = "dashed",
             size = 2,
             colour = "purple")+
  geom_vline(xintercept = 2,
             linetype = "dashed",
             size = 2)+
  xlab("Full Dose Mixture IRM Value in Solo Insecticides")+
  ylab("Frequency")+
  theme_classic()

#subset to only locations where IRM would be needed:::
irm.needed.df = subset(value.df, full.mix.value > 1)

ggplot(irm.needed.df, aes(x=full.mix.value))+
  geom_histogram(colour = "grey",
                 fill = "seagreen",
                 binwidth = 0.1)+
  geom_vline(xintercept = median(irm.needed.df$full.mix.value),
             linetype = "dashed",
             size = 2,
             colour = "coral")+
  geom_vline(xintercept = mean(irm.needed.df$full.mix.value),
             linetype = "dashed",
             size = 2,
             colour = "purple")+
  geom_vline(xintercept = 2,
             linetype = "dashed",
             size = 2)+
  xlab("Full Dose Mixture IRM Value in Solo Insecticides")+
  ylab("Frequency")+
  theme_classic()



ggplot(value.df, aes(x=half.mix.value.50))+
  geom_histogram(colour = "grey",
                 fill = "seagreen",
                 binwidth = 0.1)+
  geom_vline(xintercept = median(value.df$half.mix.value.50),
             linetype = "dashed",
             size = 2,
             colour = "coral")+
  geom_vline(xintercept = mean(value.df$half.mix.value.50),
             linetype = "dashed",
             size = 2,
             colour = "purple")+
  geom_vline(xintercept = 2,
             linetype = "dashed",
             size = 2)+
  xlab("Half Dose Mixture IRM Value in Solo Insecticides")+
  ylab("Frequency")+
  theme_classic()


df.param = cbind(value.df,
                male.insecticide.exposure,
                female.insecticide.exposure,
                heritability,
                dispersal.rate,
                intervention.coverage)

write.csv(df.param, "mixture.value.polytruncate.csv")



ggplot(df.param, aes(x=heritability, y=full.mix.value) ) +
  geom_bin2d(bins= 10) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(df.param, aes(x=heritability, y=full.mix.value) ) +
  geom_bin2d(bins= 25) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
