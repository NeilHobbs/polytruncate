rotations.df = read.csv(".//rotation_truncation.csv")
sequences.df = read.csv(".//sequence_truncation.csv")
mixtures.df = read.csv(".//mixture_truncation.csv")

sequences.df$sequence.duration = sequences.df$simulation.duration
sequences.df$sequence.peak = sequences.df$peak.resistance
sequences.df$sequence.mean = sequences.df$mean.resistance.score.

rotations.df$rotation.duration = rotations.df$simulation.duration
rotations.df$rotation.peak = rotations.df$peak.resistance
rotations.df$rotation.mean = rotations.df$mean.resistance.score.

mixtures.df$mixtures.duration = mixtures.df$simulation.duration
mixtures.df$mixtures.peak = mixtures.df$peak.resistance

#row : col

rotations.df = rotations.df[1:55000, (c(2:9, 13:15))]
sequences.df = sequences.df[1:55000, (c(2:9, 13:15))]
mixtures.df = mixtures.df[1:55000, (c(2:9, 14,15))]


rotations.sequences.df = dplyr::inner_join(rotations.df, sequences.df)
rotations.sequences.mixtures.df = dplyr::inner_join(rotations.sequences.df, mixtures.df)



##Remove any comparisons where resistance was not able to take off:
rotations.sequences.df.1 = rotations.sequences.df%>%
  dplyr::filter(rotation.peak > 1)%>%
  dplyr::filter(sequence.peak > 1)


rotations.sequences.df.1$difference.duration = rotations.sequences.df.1$sequence.duration - rotations.sequences.df.1$rotation.duration

ggplot(rotations.sequences.df.1, aes(x=difference.duration))+
  geom_histogram()+
  geom_vline(xintercept = 0)+
  facet_wrap(~cross.selection)

#Overall gives equivalent pattern as to polyres. Therefore truncation selection does
  #not appear to overly change the conclusions when comparing sequences and rotations.


##Remove any comparisons where resistance was not able to take off:
rotations.sequences.mixtures.df.1 = rotations.sequences.mixtures.df%>%
  dplyr::filter(rotation.peak > 1)%>%
  dplyr::filter(sequence.peak > 1)%>%
  dplyr::filter(mixtures.peak > 1)

rotations.sequences.mixtures.df.1$seq.mix.diff = rotations.sequences.mixtures.df.1$sequence.duration - rotations.sequences.mixtures.df.1$mixtures.duration
rotations.sequences.mixtures.df.1$rot.mix.diff = rotations.sequences.mixtures.df.1$rotation.duration - rotations.sequences.mixtures.df.1$mixtures.duration

rotations.sequences.mixtures.df.1$seq.mix.percent.diff = ((rotations.sequences.mixtures.df.1$sequence.duration - rotations.sequences.mixtures.df.1$mixtures.duration)/rotations.sequences.mixtures.df.1$sequence.duration)*100
rotations.sequences.mixtures.df.1$rot.mix.percent.diff = ((rotations.sequences.mixtures.df.1$rotation.duration - rotations.sequences.mixtures.df.1$mixtures.duration)/rotations.sequences.mixtures.df.1$rotation.duration)*100


ggplot(rotations.sequences.mixtures.df.1, aes(x=seq.mix.percent.diff))+
  geom_histogram()+
  facet_wrap(~cross.selection)+
  theme_classic()

ggplot(rotations.sequences.mixtures.df.1, aes(x=rot.mix.percent.diff))+
  geom_histogram()+
  facet_wrap(~cross.selection)


##What we are now seeing is that mixtures are not always the best strategy,
#especially when there is high fitness costs.

colnames(rotations.sequences.mixtures.df.1)

ggplot(rotations.sequences.mixtures.df.1, aes(y=rot.mix.diff, x = fitness.selection.differential))+
  geom_point()



max(rotations.sequences.mixtures.df.1$rot.mix.diff)


#get the "weirdest" parameter set:::
rotations.sequences.mixtures.df.max = rotations.sequences.mixtures.df.1%>%
  dplyr::filter(rot.mix.diff == 380)


mix.sim = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 1,
                                                                               female.fitness.cost = rotations.sequences.mixtures.df.max$fitness.selection.differential,
                                                                               male.fitness.cost = rotations.sequences.mixtures.df.max$fitness.selection.differential,
                                                                               female.insecticide.exposure = rotations.sequences.mixtures.df.max$Female.Insecticide.Exposure,
                                                                               male.insecticide.exposure = rotations.sequences.mixtures.df.max$Male.Insecticide.Exposure,
                                                                               heritability = rotations.sequences.mixtures.df.max$Heritability,
                                                                               dispersal.rate = rotations.sequences.mixtures.df.max$Dispersal,
                                                                               intervention.coverage = rotations.sequences.mixtures.df.max$Intervention.Coverage,
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
                                                                               min.cross.selection = rotations.sequences.mixtures.df.max$cross.selection,
                                                                               max.cross.selection = rotations.sequences.mixtures.df.max$cross.selection,
                                                                               deployment.type = "mixtures",
                                                                               mixture.strategy = "mix.sequential.discrete"),
                                  maximum.generations = 500, number.of.insecticides = 2)

rot.sim = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                         exposure.scaling.factor = 1,
                                                                                         female.fitness.cost = rotations.sequences.mixtures.df.max$fitness.selection.differential,
                                                                                         male.fitness.cost = rotations.sequences.mixtures.df.max$fitness.selection.differential,
                                                                                         female.insecticide.exposure = rotations.sequences.mixtures.df.max$Female.Insecticide.Exposure,
                                                                                         male.insecticide.exposure = rotations.sequences.mixtures.df.max$Male.Insecticide.Exposure,
                                                                                         heritability = rotations.sequences.mixtures.df.max$Heritability,
                                                                                         dispersal.rate = rotations.sequences.mixtures.df.max$Dispersal,
                                                                                         intervention.coverage = rotations.sequences.mixtures.df.max$Intervention.Coverage,
                                                                                         standard.deviation = 20,
                                                                                         vector.length = 1000,
                                                                                         maximum.bioassay.survival.proportion = 1,
                                                                                         michaelis.menten.slope = 1,
                                                                                         regression.coefficient = 0.48,
                                                                                         regression.intercept = 0.15,
                                                                                         maximum.generations = 500,
                                                                                         irm.strategy = "rotation",
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
                                                                                         min.cross.selection = rotations.sequences.mixtures.df.max$cross.selection,
                                                                                         max.cross.selection = rotations.sequences.mixtures.df.max$cross.selection,
                                                                                         deployment.type = "singles",
                                                                                         mixture.strategy = "mix.sequential.discrete"),
                                            maximum.generations = 500, number.of.insecticides = 2)




mix.sim = mix.sim%>%
  dplyr::filter(site == "intervention")

rot.sim = rot.sim%>%
  dplyr::filter(site == "intervention")


ggplot(rot.sim, aes(x=time.in.generations, y=resistance.score,
                    colour = insecticide.tracked))+
  geom_line()


ggplot(mix.sim, aes(x=time.in.generations, y=resistance.intensity,
                    colour = insecticide.tracked))+
  geom_line()


