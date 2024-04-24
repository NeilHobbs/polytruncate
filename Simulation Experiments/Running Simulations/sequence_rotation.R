parameter.space.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")


#need to change the sign of the fitness cost parameters:
parameter.space.df$Female.Fitness.Cost = parameter.space.df$Female.Fitness.Cost  * -1
parameter.space.df$Male.Fitness.Cost = parameter.space.df$Male.Fitness.Cost  * -1

sequence.list = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
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
                               maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  sequence.list[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, sequence.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//sequence.truncation.test.csv")




rotation.list = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
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
                                                                        min.cross.selection = 0,
                                                                        max.cross.selection = 0,
                                                                        deployment.type = "singles",
                                                                        mixture.strategy = N),
                               maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  rotation.list[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, rotation.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//rotation.truncation.test.csv")



mixture.list = list()

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
                                                                            deployment.type = "mixtures",
                                                                            mixture.strategy = "mix.sequential.discrete"),
                               maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  mixture.list[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, mixture.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//mixture.truncation.test.csv")









