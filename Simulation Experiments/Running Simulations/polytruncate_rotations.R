####
#using the same parameters as for polyres, except:
#1. fitness costs - now recalculated as a fixed selection differential.
#2. Standard deviaton and Exposure Scaling Factor to calibrate the sims


parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.csv")
parameter.space.df = parameter.space[1:5000, 2:7]


#Convert fitness cost to a fixed selection differential
m = parameter.space.df$Male.Insecticide.Exposure
x = parameter.space.df$Female.Insecticide.Exposure
h = parameter.space.df$Heritability

R = (((m + 1)*x)/2) * 10 * h ##still need beta from polyres

parameter.space.df$fitness.selection.differential = ((R / h) * 2) * parameter.space.df$Fitness.Cost

parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df)

parameter.space.df$cross.selection = c(rep(-0.5, 5000), rep(-0.4, 5000), rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000),
                                       rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000), rep(0.4, 5000), rep(0.5, 5000))


rotation.list = list()

for(v in 1:nrow(parameter.space.df)){

  temp = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 1,
                                                                               female.fitness.cost = parameter.space.df$fitness.selection.differential[v],
                                                                               male.fitness.cost = parameter.space.df$fitness.selection.differential[v],
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
                                                                               min.cross.selection = parameter.space.df$cross.selection[v],
                                                                               max.cross.selection = parameter.space.df$cross.selection [v],
                                                                               deployment.type = "singles"),
                                  maximum.generations = 500, number.of.insecticides = 2)

  temp_intervention = temp%>% #only need to know for the intervention site as this is where the decisions are made from.
    dplyr::filter(site == "intervention")

  simulation.duration = max(temp_intervention$time.in.generations) #Duration of simulation


  peak.resistance = max(temp_intervention$resistance.score)

  average.resistance.score = as.vector(temp_intervention%>%
                                         dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                         summarise(mean(resistance.score)))


  temp_2 = data.frame(simulation.duration, average.resistance.score, peak.resistance)


  rotation.list[[v]] = temp_2

  if(v %% 10 == 0){print(v)}
}


rotation.truncation.df = do.call(rbind, rotation.list)

final.rotation.df = cbind(parameter.space.df, rotation.truncation.df)


write.csv(final.rotation.df, ".//rotation_truncation.csv")
