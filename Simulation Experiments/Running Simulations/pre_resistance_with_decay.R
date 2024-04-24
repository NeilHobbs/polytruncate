base.decay = rep(c(0.005, 0.015, 0.025), 3)
threshold.gens = c(10, 10, 10, 15, 15, 15, 20, 20, 20)

solo.decay = c()
for(i in 1:9){
A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                          exposure.scaling.factor = 1,
                                                                          female.fitness.cost = 0,
                                                                          male.fitness.cost = 0,
                                                                          female.insecticide.exposure = 0.8,
                                                                          male.insecticide.exposure = 0.8,
                                                                          heritability = 0.2,
                                                                          dispersal.rate = 0.3,
                                                                          intervention.coverage = 0.8,
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
                                                                          deployment.frequency = 30, #minimum deployment frequency
                                                                          maximum.resistance.value = 25000,
                                                                          starting.refugia.resistance.score = 0,
                                                                          starting.intervention.resistance.score = 0,
                                                                          applied.insecticide.dose = 1,
                                                                          recommended.insecticide.dose = 1,
                                                                          threshold.generations = threshold.gens[i],#no decay, so this value does not matter
                                                                          base.efficacy.decay.rate = base.decay[i],
                                                                          rapid.decay.rate = 0.08,
                                                                          population.suppression = FALSE,
                                                                          min.cross.selection = 0,
                                                                          max.cross.selection = 0,
                                                                          deployment.type = "singles",
                                                                          mixture.strategy = NA),
                             maximum.generations = 500, number.of.insecticides = 1)

B = A%>%
  dplyr::filter(site == "intervention")

sim.duration = max(B$time.in.generations)

solo.decay[i] = sim.duration
}



base.decay.1 = rep(rep(c(rep(0.005, 27), rep(0.015, 27), rep(0.025, 27)), 4), 4)
threshold.gens.1 = rep(rep(rep(c(rep(10, 9), rep(15, 9), rep(20, 9)), 3), 4), 4)
base.decay.2 = rep(rep(rep(c(rep(0.005, 3), rep(0.015, 3), rep(0.025, 3)), 9), 4), 4)
threshold.gens.2 = rep(rep(rep(c(10, 15, 20), 27), 4), 4)

start.resistance.2 = c(rep(rep(0, 81), 4), rep(rep(100, 81), 4), rep(rep(900, 81), 4), rep(rep(3600, 81), 4))
dose.1 = rep(c(rep(1, 81), rep(1, 81), rep(0.5, 81), rep(0.5, 81)), 4)
dose.2 = rep(c(rep(1, 81), rep(0.5, 81), rep(1, 81), rep(0.5, 81)), 4)


mixture.decay = c()
for(i in 1:1296){
  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                          exposure.scaling.factor = 1,
                                                                                          female.fitness.cost = 0,
                                                                                          male.fitness.cost = 0,
                                                                                          female.insecticide.exposure = 0.8,
                                                                                          male.insecticide.exposure = 0.8,
                                                                                          heritability = 0.3,
                                                                                          dispersal.rate = 0.2,
                                                                                          intervention.coverage = 0.8,
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
                                                                                          deployment.frequency = 30, #minimum deployment frequency
                                                                                          maximum.resistance.value = 25000,
                                                                                          starting.refugia.resistance.score = c(0, start.resistance.2[i]),
                                                                                          starting.intervention.resistance.score = c(0, start.resistance.2[i]),
                                                                                          applied.insecticide.dose = c(dose.1[i], dose.2[i]),
                                                                                          recommended.insecticide.dose = 1,
                                                                                          threshold.generations = c(threshold.gens.1[i], threshold.gens.2[i]),#no decay, so this value does not matter
                                                                                          base.efficacy.decay.rate = c(base.decay.1[i], base.decay.2[i]),
                                                                                          rapid.decay.rate = 0.08,
                                                                                          population.suppression = FALSE,
                                                                                          min.cross.selection = 0,
                                                                                          max.cross.selection = 0,
                                                                                          deployment.type = "mixtures",
                                                                                          mixture.strategy = "mix.sequential.discrete"),
                                             maximum.generations = 500, number.of.insecticides = 2)


  B = A%>%
    dplyr::filter(site == "intervention")

 sim.duration = max(B$time.in.generations)

 mixture.decay[i] = sim.duration

 if(i %% 10 == 0){print(i)}

}


##Now to match up the simulations:::
mixture.decay.df = data.frame(mixture.decay,
                              base.decay.1,
                              base.decay.2,
                              threshold.gens.1,
                              threshold.gens.2,
                              dose.1,
                              dose.2,
                              start.resistance.2)

solo.decay.df = data.frame(solo.decay,
                           base.decay,
                           threshold.gens)

solo.decay.df = solo.decay.df%>%
  dplyr::rename(base.decay.1 = "base.decay")%>%
  dplyr::rename(threshold.gens.1 = "threshold.gens")


compare.decay.df = inner_join(mixture.decay.df, solo.decay.df)


compare.decay.df$change.duration = compare.decay.df$mixture.decay - compare.decay.df$solo.decay
compare.decay.df$prop.change.duration = (compare.decay.df$mixture.decay - compare.decay.df$solo.decay)/compare.decay.df$mixture.decay

compare.decay.df$dosing = ifelse(compare.decay.df$dose.1 == 1 &
                                   compare.decay.df$dose.2 == 1,
                                 yes = "FD_FD",
                                 no = ifelse(compare.decay.df$dose.1 == 1 &
                                               compare.decay.df$dose.2 == 0.5,
                                             yes = "FD_HD",
                                             no = ifelse(compare.decay.df$dose.1 == 0.5 &
                                                           compare.decay.df$dose.2 == 1,
                                                         yes = "HD_FD",
                                                         no = "HD_HD")))


compare.decay.df$start.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                trait.mean = compare.decay.df$start.resistance.2,
                                                                                michaelis.menten.slope = 1,
                                                                                half.population.bioassay.survival.resistance = 900) * 100

ggplot(compare.decay.df, aes(x=prop.change.duration*100,
                             fill = dosing))+
  geom_histogram(binwidth = 10,
                 colour = "black")+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey")+
  xlab("Change in Operational Lifespan")+
  guides(fill=guide_legend(title="Dosing:Novel_Old"))+
  theme_classic()+
  facet_grid(start.bioassay ~ .)


compare.decay.df.1 = compare.decay.df%>%
  dplyr::filter(threshold.gens.1 == 15)%>%
  dplyr::filter(base.decay.1 == 0.015)


ggplot(compare.decay.df.1, aes(x=base.decay.2,
                               y= change.duration/10,
                               colour = as.factor(threshold.gens.2)))+
  geom_point()+
  geom_line()+
  xlab("Base Decay Rate Old")+
  ylab("Change in Operational Lifespan (Years)")+
  ggtitle("Truncation Selection. Novel: base decay = 0.015; threshold generations = 15")+
  guides(colour=guide_legend(title = "Threshold Generations Old"))+
  facet_grid(start.bioassay~dosing)+
  theme_bw()+
  theme(legend.position = "bottom")




ggplot(compare.decay.df, aes(x=start.bioassay,
                             y=change.duration/10,
                             colour = dosing))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0,
             linetype="dashed",
             colour = "black")+
  ylab("Change in Operational Lifespan (Years)")+
  xlab("Start Bioassay Old (%)")+
  facet_grid(threshold.gens.1 + base.decay.1 ~ threshold.gens.2 + base.decay.2)+
  theme_bw()+
  theme(legend.position = "bottom")



base.decay = rep(c(0.005, 0.015, 0.025), 3)
threshold.gens = c(10, 10, 10, 15, 15, 15, 20, 20, 20)

solo.decay.scaled = c()
for(i in 1:9){
  A = get_simulation_dataframe(simulation.array = run_simulation_truncation_test(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 1,
                                                                            female.fitness.cost = 0,
                                                                            male.fitness.cost = 0,
                                                                            female.insecticide.exposure = 0.8,
                                                                            male.insecticide.exposure = 0.8,
                                                                            heritability = 0.2,
                                                                            dispersal.rate = 0.3,
                                                                            intervention.coverage = 0.8,
                                                                            standard.deviation = NA,
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
                                                                            deployment.frequency = 30, #minimum deployment frequency
                                                                            maximum.resistance.value = 25000,
                                                                            starting.refugia.resistance.score = 0,
                                                                            starting.intervention.resistance.score = 0,
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = threshold.gens[i],#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = base.decay[i],
                                                                            rapid.decay.rate = 0.08,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA,
                                                                            z.sd.coefficient = 0.413911,
                                                                            z.sd.intercept = 20.186745),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  sim.duration = max(B$time.in.generations)

  solo.decay.scaled[i] = sim.duration
}



base.decay.1 = rep(rep(c(rep(0.005, 27), rep(0.015, 27), rep(0.025, 27)), 4), 4)
threshold.gens.1 = rep(rep(rep(c(rep(10, 9), rep(15, 9), rep(20, 9)), 3), 4), 4)
base.decay.2 = rep(rep(rep(c(rep(0.005, 3), rep(0.015, 3), rep(0.025, 3)), 9), 4), 4)
threshold.gens.2 = rep(rep(rep(c(10, 15, 20), 27), 4), 4)

start.resistance.2 = c(rep(rep(0, 81), 4), rep(rep(100, 81), 4), rep(rep(900, 81), 4), rep(rep(3600, 81), 4))
dose.1 = rep(c(rep(1, 81), rep(1, 81), rep(0.5, 81), rep(0.5, 81)), 4)
dose.2 = rep(c(rep(1, 81), rep(0.5, 81), rep(1, 81), rep(0.5, 81)), 4)


mixture.decay.scaled = c()
for(i in 1:1296){
  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation_test(number.of.insecticides = 2,
                                                                                     exposure.scaling.factor = 1,
                                                                                     female.fitness.cost = 0,
                                                                                     male.fitness.cost = 0,
                                                                                     female.insecticide.exposure = 0.8,
                                                                                     male.insecticide.exposure = 0.8,
                                                                                     heritability = 0.3,
                                                                                     dispersal.rate = 0.2,
                                                                                     intervention.coverage = 0.8,
                                                                                     standard.deviation = NA,
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
                                                                                     deployment.frequency = 30, #minimum deployment frequency
                                                                                     maximum.resistance.value = 25000,
                                                                                     starting.refugia.resistance.score = c(0, start.resistance.2[i]),
                                                                                     starting.intervention.resistance.score = c(0, start.resistance.2[i]),
                                                                                     applied.insecticide.dose = c(dose.1[i], dose.2[i]),
                                                                                     recommended.insecticide.dose = 1,
                                                                                     threshold.generations = c(threshold.gens.1[i], threshold.gens.2[i]),#no decay, so this value does not matter
                                                                                     base.efficacy.decay.rate = c(base.decay.1[i], base.decay.2[i]),
                                                                                     rapid.decay.rate = 0.08,
                                                                                     population.suppression = FALSE,
                                                                                     min.cross.selection = 0,
                                                                                     max.cross.selection = 0,
                                                                                     deployment.type = "mixtures",
                                                                                     mixture.strategy = "mix.sequential.discrete",
                                                                                     z.sd.coefficient = 0.413911,
                                                                                     z.sd.intercept = 20.186745),
                                        maximum.generations = 500, number.of.insecticides = 2)


  B = A%>%
    dplyr::filter(site == "intervention")

  sim.duration = max(B$time.in.generations)

  mixture.decay.scaled[i] = sim.duration

  if(i %% 10 == 0){print(i)}

}


##Now to match up the simulations:::
mixture.decay.scaled.df = data.frame(mixture.decay.scaled,
                              base.decay.1,
                              base.decay.2,
                              threshold.gens.1,
                              threshold.gens.2,
                              dose.1,
                              dose.2,
                              start.resistance.2)

solo.decay.scaled.df = data.frame(solo.decay.scaled,
                           base.decay,
                           threshold.gens)

solo.decay.scaled.df = solo.decay.scaled.df%>%
  dplyr::rename(base.decay.1 = "base.decay")%>%
  dplyr::rename(threshold.gens.1 = "threshold.gens")


compare.decay.scaled.df = inner_join(mixture.decay.scaled.df, solo.decay.scaled.df)


compare.decay.scaled.df$change.duration = compare.decay.scaled.df$mixture.decay.scaled - compare.decay.scaled.df$solo.decay.scaled

compare.decay.scaled.df$dosing = ifelse(compare.decay.scaled.df$dose.1 == 1 &
                                          compare.decay.scaled.df$dose.2 == 1,
                                 yes = "FD_FD",
                                 no = ifelse(compare.decay.scaled.df$dose.1 == 1 &
                                               compare.decay.scaled.df$dose.2 == 0.5,
                                             yes = "FD_HD",
                                             no = ifelse(compare.decay.scaled.df$dose.1 == 0.5 &
                                                           compare.decay.scaled.df$dose.2 == 1,
                                                         yes = "HD_FD",
                                                         no = "HD_HD")))


compare.decay.scaled.df$start.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                trait.mean = compare.decay.scaled.df$start.resistance.2,
                                                                                michaelis.menten.slope = 1,
                                                                                half.population.bioassay.survival.resistance = 900) * 100

library(scales)

ggplot(compare.decay.scaled.df, aes(x=change.duration/10,
                             fill = dosing))+
  geom_histogram(binwidth = 1,
                 colour = "black")+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey")+
  scale_y_continuous(labels = percent)+
  xlab("Increase in Operational Lifespan (Years)")+
  guides(fill=guide_legend(title="Dosing:Novel_Old"))+
  theme_classic()+
  facet_grid(start.bioassay ~ .)


compare.decay.scaled.df.1 = compare.decay.scaled.df%>%
  dplyr::filter(threshold.gens.1 == 15)%>%
  dplyr::filter(base.decay.1 == 0.015)


ggplot(compare.decay.scaled.df.1, aes(x=base.decay.2,
                               y= change.duration/10,
                               colour = as.factor(threshold.gens.2)))+
  geom_point()+
  geom_line()+
  xlab("Base Decay Rate Old")+
  ylab("Change in Operational Lifespan (Years)")+
  ggtitle("Truncation Selection. Novel: base decay = 0.015; threshold generations = 15")+
  guides(colour=guide_legend(title = "Threshold Generations Old"))+
  facet_grid(start.bioassay~dosing)+
  theme_bw()+
  theme(legend.position = "bottom")




ggplot(compare.decay.scaled.df, aes(x=start.bioassay,
                             y=change.duration/10,
                             colour = dosing))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0,
             linetype="dashed",
             colour = "black")+
  ylab("Change in Operational Lifespan (Years)")+
  xlab("Start Bioassay Old (%)")+
  facet_grid(threshold.gens.1 + base.decay.1 ~ threshold.gens.2 + base.decay.2)+
  theme_bw()+
  theme(legend.position = "bottom")







