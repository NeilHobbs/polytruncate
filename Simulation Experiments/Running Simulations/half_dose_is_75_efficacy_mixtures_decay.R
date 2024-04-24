#Hlaf dose is 75% efficacy.

library(devtools)
load_all()

parameter_space_smooth = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")
parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

# set1 = 1:250
# set2 = 251:500
# set3 = 501:750
# set4 = 751:1000
# set5 = 1001:1250
# set6 = 1251:1500
# set7 = 1501:1750
# set8 = 1751:2000
# set9 = 2001:2250
# set10 = 2251:2500



half_dose_is_75_efficacy_mixtures_decay = function(parameter.space.df,
                                                   min_i,
                                                   max_i){


  base.decay.1 = rep(rep(rep(c(0.005, 0.015, 0.025), 3), 3), 4)

  base.decay.2 = rep(rep(c(rep(0.005, 3), rep(0.015, 3), rep(0.025, 3)), 3), 4)

  #threshold gens should be equal; this is more to do with the physical structure of the net etc - so would be the same for both insecticides.
  threshold.gens = rep(c(rep(10, 9), rep(15, 9), rep(20, 9)), 4)

  start.resistance.2 = c(rep(0, 27), rep(100, 27), rep(900, 27), rep(3600, 27))
  dose.1 = rep(0.75, 108)
  dose.2 = rep(0.75, 108)



  mixture.list = list()
  mixture.pyrethroid = c()
  mixture.novel = c()

  for(j in min_i:max_i){
    for(i in 1:108){

      sim.df = get_simulation_dataframe_mixtures(run_simulation_truncation(number.of.insecticides = 2,
                                                                           exposure.scaling.factor = 1,
                                                                           female.fitness.cost = 0,
                                                                           male.fitness.cost = 0,
                                                                           female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j],
                                                                           male.insecticide.exposure = parameter_space_smooth$Male.Insecticide.Exposure[j],
                                                                           heritability = parameter_space_smooth$Heritability[j],
                                                                           dispersal.rate = parameter_space_smooth$Dispersal[j],
                                                                           intervention.coverage = parameter_space_smooth$Intervention.Coverage[j],
                                                                           standard.deviation,
                                                                           z.sd.coefficient = 0.4,
                                                                           z.sd.intercept = 18,
                                                                           sd.scaled = TRUE, #TRuE or FALSE. False is the default
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
                                                                           starting.refugia.resistance.score = c(0, start.resistance.2[i]),
                                                                           starting.intervention.resistance.score = c(0, start.resistance.2[i]),
                                                                           applied.insecticide.dose = c(dose.1[i], dose.2[i]),
                                                                           recommended.insecticide.dose= 1,
                                                                           threshold.generations = threshold.gens[i],
                                                                           base.efficacy.decay.rate = c(base.decay.1[i], base.decay.2[i]),
                                                                           rapid.decay.rate = 0.08,
                                                                           population.suppression = FALSE,
                                                                           min.cross.selection = 0,
                                                                           max.cross.selection = 0,
                                                                           deployment.type = "mixtures",
                                                                           mixture.strategy = "mix.sequential.discrete"),
                                                 number.of.insecticides = 2,
                                                 maximum.generations = 200)



      sim.df.novel = sim.df%>%
        dplyr::filter(site == "intervention")%>%
        dplyr::filter(insecticide.tracked == 1)


      sim.df.pyrethroid= sim.df%>%
        dplyr::filter(site == "intervention")%>%
        dplyr::filter(insecticide.tracked == 2)

      # outcomes:
      #1. Change resistance "novel"

      mixture.novel[i] = max(sim.df.novel$resistance.intensity)

      #2. Change resistance "pyrethroid"

      mixture.pyrethroid[i] = max(sim.df.pyrethroid$resistance.intensity)

      print(c(i, j))
    }


    ##Now to match up the simulations:::
    mixture.df = data.frame(mixture.novel,
                            mixture.pyrethroid,
                            base.decay.1,
                            base.decay.2,
                            threshold.gens,
                            dose.1,
                            dose.2,
                            start.resistance.2)





    heritability = rep(parameter.space.df$Heritability[j], 108)
    male.exposure = rep(parameter.space.df$Male.Insecticide.Exposure[j], 108)
    female.exposure = rep(parameter.space.df$Female.Insecticide.Exposure[j], 108)
    intervention.coverage = rep(parameter.space.df$Intervention.Coverage[j], 108)
    dispersal = rep(parameter.space.df$Dispersal[j], 108)

    mixture.df = data.frame(mixture.df,
                            heritability,
                            male.exposure,
                            female.exposure,
                            intervention.coverage,
                            dispersal)

    mixture.list[[j]] = mixture.df

    print(c(i, j))

  }
  return(mixture.list)

}

set1 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 1,
                                               max_i = 250)
mixtures.truncation.set.1 = do.call(rbind, set1)

write.csv(mixtures.truncation.set.1, "mixtures.truncation.75efficacy.set.1.csv")

set2 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 251,
                                               max_i = 500)
mixtures.truncation.set.2 = do.call(rbind, set2)

write.csv(mixtures.truncation.set.2, "mixtures.truncation.75efficacy.set.2.csv")


set3 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 501,
                                               max_i = 750)
mixtures.truncation.set.3 = do.call(rbind, set3)

write.csv(mixtures.truncation.set.3, "mixtures.truncation.75efficacy.set.3.csv")


set4 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 751,
                                               max_i = 1000)
mixtures.truncation.set.4 = do.call(rbind, set4)

write.csv(mixtures.truncation.set.4, "mixtures.truncation.75efficacy.set.4.csv")



set5 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 1001,
                                               max_i = 1250)
mixtures.truncation.set.5 = do.call(rbind, set5)

write.csv(mixtures.truncation.set.5, "mixtures.truncation.75efficacy.set.5.csv")

set6 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 1251,
                                               max_i = 1500)
mixtures.truncation.set.6 = do.call(rbind, set6)

write.csv(mixtures.truncation.set.6, "mixtures.truncation.75efficacy.set.6.csv")





set7 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 1501,
                                               max_i = 1750)
mixtures.truncation.set.7 = do.call(rbind, set7)

write.csv(mixtures.truncation.set.7, "mixtures.truncation.75efficacy.set.7.csv")


set8 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 1751,
                                               max_i = 2000)
mixtures.truncation.set.8 = do.call(rbind, set8)

write.csv(mixtures.truncation.set.8, "mixtures.truncation.75efficacy.set.8.csv")


set9 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 2001,
                                               max_i = 2250)
mixtures.truncation.set.9 = do.call(rbind, set9)

write.csv(mixtures.truncation.set.9, "mixtures.truncation.75efficacy.set.9.csv")


set10 = half_dose_is_75_efficacy_mixtures_decay(parameter.space.df = parameter_space_smooth,
                                               min_i = 2251,
                                               max_i = 2500)
mixtures.truncation.set.10 = do.call(rbind, set10)

write.csv(mixtures.truncation.set.10, "mixtures.truncation.75efficacy.set.10.csv")
