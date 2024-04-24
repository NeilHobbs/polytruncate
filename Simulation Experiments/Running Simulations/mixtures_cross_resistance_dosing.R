library(devtools)
load_all()

cross.resistance = rep(c(rep(-0.5, 2500),
                         rep(-0.4, 2500),
                         rep(-0.3, 2500),
                         rep(-0.2, 2500),
                         rep(-0.1, 2500),
                         rep(0, 2500),
                         rep(0.1, 2500),
                         rep(0.2, 2500),
                         rep(0.3, 2500),
                         rep(0.4, 2500),
                         rep(0.5, 2500)), 3)

dose = c(rep(0.5, 27500), rep(0.75, 27500), rep(1, 27500))

parameter_space_smooth = read.csv(("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv"))

parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

parameter_space_smooth = parameter_space_smooth[rep(1:2500, 33),]

insecticide.i = c()
insecticide.j = c()



for(i in 1:82500){
sim.df = get_simulation_dataframe_mixtures(run_simulation_truncation(number.of.insecticides = 2,
                                                                     exposure.scaling.factor = 1,
                                                                     female.fitness.cost = 0,
                                                                     male.fitness.cost = 0,
                                                                     female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[i],
                                                                     male.insecticide.exposure = parameter_space_smooth$Male.Insecticide.Exposure[i],
                                                                     heritability = parameter_space_smooth$Heritability[i],
                                                                     dispersal.rate = parameter_space_smooth$Dispersal[i],
                                                                     intervention.coverage = parameter_space_smooth$Intervention.Coverage[i],
                                                                     standard.deviation = 20,
                                                                     z.sd.coefficient = 0.4,
                                                                     z.sd.intercept = 18,
                                                                     sd.scaled = FALSE, #TRuE or FALSE. False is the default
                                                                     vector.length= 250,
                                                                     maximum.bioassay.survival.proportion = 1,
                                                                     michaelis.menten.slope = 1,
                                                                     regression.coefficient = 0.48,
                                                                     regression.intercept = 0.15,
                                                                     maximum.generations = 200,
                                                                     irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                                     half.population.bioassay.survival.resistance = 900,
                                                                     withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                     return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                     deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                     maximum.resistance.value = 900000,
                                                                     starting.refugia.resistance.score = 0,
                                                                     starting.intervention.resistance.score = 0,
                                                                     applied.insecticide.dose = dose[i],
                                                                     recommended.insecticide.dose= 1,
                                                                     threshold.generations = 10,
                                                                     base.efficacy.decay.rate = 0,
                                                                     rapid.decay.rate = 0,
                                                                     population.suppression = FALSE,
                                                                     min.cross.selection = cross.resistance[i],
                                                                     max.cross.selection = cross.resistance[i],
                                                                     deployment.type = "mixtures",
                                                                     mixture.strategy = "mix.sequential.discrete"),
                                           number.of.insecticides = 2,
                                           maximum.generations = 200)



insecticide.i.df = sim.df%>%
  dplyr::filter(site == "intervention")%>%
  dplyr::filter(insecticide.tracked == 1)


insecticide.j.df = sim.df%>%
  dplyr::filter(site == "intervention")%>%
  dplyr::filter(insecticide.tracked == 2)

insecticide.i[i] = max(insecticide.i.df$resistance.intensity)
insecticide.j[i] = max(insecticide.j.df$resistance.intensity)


print(i)
}

parameter_space_smooth$insecticide.i = insecticide.i
parameter_space_smooth$insecticide.j = insecticide.j
parameter_space_smooth$Male.Fitness.Cost = 0
parameter_space_smooth$Female.Fitness.Cost = 0
parameter_space_smooth$dose = dose
parameter_space_smooth$cross.resistance = cross.resistance

write.csv(parameter_space_smooth, "part.3.mixture.cross.resistance.csv")

##solo deployments:::
cross.resistance = rep(c(rep(-0.5, 2500),
                         rep(-0.4, 2500),
                         rep(-0.3, 2500),
                         rep(-0.2, 2500),
                         rep(-0.1, 2500),
                         rep(0, 2500),
                         rep(0.1, 2500),
                         rep(0.2, 2500),
                         rep(0.3, 2500),
                         rep(0.4, 2500),
                         rep(0.5, 2500)), 1)

parameter_space_smooth = read.csv(("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv"))

parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

parameter_space_smooth = parameter_space_smooth[rep(1:2500, 11),]

parameter_space_smooth$Female.Fitness.Cost = 0
parameter_space_smooth$Male.Fitness.Cost = 0

deployed.insecticide = c()
not.deployed.insecticide = c()

for(i in 1:27500){
  sim.df = get_simulation_dataframe(run_simulation_truncation(number.of.insecticides = 2,
                                                                       exposure.scaling.factor = 1,
                                                                       female.fitness.cost = 0,
                                                                       male.fitness.cost = 0,
                                                                       female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[i],
                                                                       male.insecticide.exposure = parameter_space_smooth$Male.Insecticide.Exposure[i],
                                                                       heritability = parameter_space_smooth$Heritability[i],
                                                                       dispersal.rate = parameter_space_smooth$Dispersal[i],
                                                                       intervention.coverage = parameter_space_smooth$Intervention.Coverage[i],
                                                                       standard.deviation = 20,
                                                                       z.sd.coefficient = 0.4,
                                                                       z.sd.intercept = 18,
                                                                       sd.scaled = FALSE, #TRuE or FALSE. False is the default
                                                                       vector.length= 250,
                                                                       maximum.bioassay.survival.proportion = 1,
                                                                       michaelis.menten.slope = 1,
                                                                       regression.coefficient = 0.48,
                                                                       regression.intercept = 0.15,
                                                                       maximum.generations = 200,
                                                                       irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                                       half.population.bioassay.survival.resistance = 900,
                                                                       withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                       deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                       maximum.resistance.value = 900000,
                                                                       starting.refugia.resistance.score = 0,
                                                                       starting.intervention.resistance.score = 0,
                                                                       applied.insecticide.dose = 1,
                                                                       recommended.insecticide.dose= 1,
                                                                       threshold.generations = 10,
                                                                       base.efficacy.decay.rate = 0,
                                                                       rapid.decay.rate = 0,
                                                                       population.suppression = FALSE,
                                                                       min.cross.selection = cross.resistance[i],
                                                                       max.cross.selection = cross.resistance[i],
                                                                       deployment.type = "singles",
                                                                       mixture.strategy = NA),
                                             number.of.insecticides = 2,
                                             maximum.generations = 200)

  insecticide.i.df = sim.df%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 1)


  insecticide.j.df = sim.df%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 2)

  deployed.insecticide[i] = max(insecticide.i.df$resistance.score)
  not.deployed.insecticide[i] = max(insecticide.j.df$resistance.score)

  print(i)
}

parameter_space_smooth$deployed.insecticide = deployed.insecticide
parameter_space_smooth$not.deployed.insecticide = not.deployed.insecticide
parameter_space_smooth$Male.Fitness.Cost = 0
parameter_space_smooth$Female.Fitness.Cost = 0
parameter_space_smooth$cross.resistance = cross.resistance

write.csv(parameter_space_smooth, "part.3.solo.cross.resistance.csv")


parameter_space_smooth = read.csv(("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv"))

parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

parameter_space_smooth = parameter_space_smooth[rep(1:2500, 11),]

parameter_space_smooth$Female.Fitness.Cost = 0
parameter_space_smooth$Male.Fitness.Cost = 0

insecticide.i = c()
insecticide.j = c()

for(i in 1:27500){
  sim.df = get_simulation_dataframe(run_simulation_truncation(number.of.insecticides = 2,
                                                              exposure.scaling.factor = 1,
                                                              female.fitness.cost = 0,
                                                              male.fitness.cost = 0,
                                                              female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[i],
                                                              male.insecticide.exposure = parameter_space_smooth$Male.Insecticide.Exposure[i],
                                                              heritability = parameter_space_smooth$Heritability[i],
                                                              dispersal.rate = parameter_space_smooth$Dispersal[i],
                                                              intervention.coverage = parameter_space_smooth$Intervention.Coverage[i],
                                                              standard.deviation = 20,
                                                              z.sd.coefficient = 0.4,
                                                              z.sd.intercept = 18,
                                                              sd.scaled = FALSE, #TRuE or FALSE. False is the default
                                                              vector.length= 250,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              maximum.generations = 200,
                                                              irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                              return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                              maximum.resistance.value = 900000,
                                                              starting.refugia.resistance.score = 0,
                                                              starting.intervention.resistance.score = 0,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose= 1,
                                                              threshold.generations = 10,
                                                              base.efficacy.decay.rate = 0,
                                                              rapid.decay.rate = 0,
                                                              population.suppression = FALSE,
                                                              min.cross.selection = cross.resistance[i],
                                                              max.cross.selection = cross.resistance[i],
                                                              deployment.type = "singles",
                                                              mixture.strategy = NA),
                                    number.of.insecticides = 2,
                                    maximum.generations = 200)

  insecticide.i.df = sim.df%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 1)


  insecticide.j.df = sim.df%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 2)

  insecticide.i[i] = max(insecticide.i.df$resistance.score)
  insecticide.j[i] = max(insecticide.j.df$resistance.score)

  print(i)
}

parameter_space_smooth$insecticide.i = insecticide.i
parameter_space_smooth$insecticide.j = insecticide.j
parameter_space_smooth$Male.Fitness.Cost = 0
parameter_space_smooth$Female.Fitness.Cost = 0
parameter_space_smooth$cross.resistance = cross.resistance

write.csv(parameter_space_smooth, "part.3.rotation.cross.resistance.csv")








