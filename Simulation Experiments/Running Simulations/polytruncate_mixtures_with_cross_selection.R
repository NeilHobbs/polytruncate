####
#using the same parameters as for polyres, except:
#1. fitness costs - now recalculated as a fixed selection differential.
#2. Standard deviaton and Exposure Scaling Factor to calibrate the sims


parameter.space.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")

parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df)

parameter.space.df$cross.selection = c(rep(-0.5, 5000), rep(-0.4, 5000), rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000),
                                       rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000), rep(0.4, 5000), rep(0.5, 5000))


insecticide.1.peak = c()
insecticide.1.mean = c()
insecticide.2.peak = c()
insecticide.2.mean = c()
simulation.duration = c()

for(v in 1:55000){
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
                                                                                     sd.scaled = FALSE,
                                                                                     z.sd.coefficient = NA,
                                                                                     z.sd.intercept = NA,
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
                                                                                     maximum.resistance.value = 90000,
                                                                                     starting.refugia.resistance.score = 0,
                                                                                     starting.intervention.resistance.score = 0,
                                                                                     applied.insecticide.dose = 1,
                                                                                     recommended.insecticide.dose = 1,
                                                                                     threshold.generations = 10,#no decay, so this value does not matter
                                                                                     base.efficacy.decay.rate = 0,
                                                                                     rapid.decay.rate = 0,
                                                                                     population.suppression = FALSE,
                                                                                     min.cross.selection = parameter.space.df$cross.selection[v],
                                                                                     max.cross.selection = parameter.space.df$cross.selection[v],
                                                                                     deployment.type = "mixtures",
                                                                                     mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 500, number.of.insecticides = 2)


  insecticide.1 = A%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 1)

  insecticide.2 = A%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 2)

  insecticide.1.peak[v] = max(insecticide.1$resistance.intensity)
  insecticide.1.mean[v] = mean(insecticide.1$resistance.intensity)
  insecticide.2.peak[v] =  max(insecticide.2$resistance.intensity)
  insecticide.2.mean[v] =  mean(insecticide.2$resistance.intensity)
  simulation.duration[v] = max(A$time.in.generations)

  if(v %% 10 == 0){print(v)}

}

mixture.truncation.df = rbind(parameter.space.df, insecticide.1.mean, insecticide.1.peak, insecticide.2.mean, insecticide.2.peak, simulation.duration)

final.mixture.df = cbind(parameter.space.df, mixture.truncation.df)


write.csv(final.mixture.df, ".//mixture.truncation.with.cross.selection.csv")


