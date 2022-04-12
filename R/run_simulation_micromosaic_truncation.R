

run_simulation_micromosaic_truncation = function(number.of.insecticides,
                                             maximum.generations,
                                             starting.intervention.resistance.score,
                                             applied.insecticide.dose,
                                             recommended.insecticide.dose,
                                             threshold.generations,
                                             base.efficacy.decay.rate,
                                             rapid.decay.rate,
                                             deployment.interval,
                                             max.cycles,
                                             intervention.coverage.1,
                                             intervention.coverage.2,
                                             standard.deviation,
                                             vector.length,
                                             female.insecticide.exposure,
                                             male.insecticide.exposure,
                                             heritability,
                                             regression.coefficient,
                                             regression.intercept,
                                             exposure.scaling.factor,
                                             male.fitness.cost,
                                             half.population.bioassay.survival.resistance,
                                             michaelis.menten.slope,
                                             maximum.bioassay.survival.proportion,
                                             cross.selection
){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = NA,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = number.of.insecticides)

  #Make a vector of the available insecticides@
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  #Make a dataframe of the insecticide parameters:
  insecticide.parameters.df = create_insecticide_parameters_dataframe(number.of.insecticides = number.of.insecticides,
                                                                      applied.insecticide.dose = applied.insecticide.dose,
                                                                      recommended.insecticide.dose = recommended.insecticide.dose,
                                                                      threshold.generation = threshold.generations,
                                                                      base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                      rapid.decay.rate = rapid.decay.rate,
                                                                      heritability = heritability)



  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector.1 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                         threshold.generations = insecticide.parameters.df[1,4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                         rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                         deployment.frequency = deployment.interval), (maximum.generations/deployment.interval))


  #The first insecticide deployed is always insecticide 2
  insecticide.efficacy.vector.2 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[2,2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[2,3],
                                                                         threshold.generations = insecticide.parameters.df[2,4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[2,5],
                                                                         rapid.decay.rate = insecticide.parameters.df[2,6],
                                                                         deployment.frequency = deployment.interval), (maximum.generations/deployment.interval))


  for(generation in 2:maximum.generations){

    male.selection.differentials = perform_male_truncation_selection_micro_mosaics(trait.mean.1 = sim.array["intervention",1, generation-1],
                                                                                   trait.mean.2 = sim.array["intervention", 2, generation-1],
                                                                                   standard.deviation = standard.deviation,
                                                                                   vector.length = vector.length,
                                                                                   female.exposure = female.insecticide.exposure,
                                                                                   male.exposure = male.insecticide.exposure,
                                                                                   coverage.1 = intervention.coverage.1,
                                                                                   coverage.2 = intervention.coverage.2,
                                                                                   heritability = heritability,
                                                                                   exposure.scaling.factor = exposure.scaling.factor,
                                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                                   regression.coefficient = regression.coefficient,
                                                                                   regression.intercept = regression.intercept,
                                                                                   current.insecticide.efficacy.1 = insecticide.efficacy.vector.1[generation],
                                                                                   current.insecticide.efficacy.2 = insecticide.efficacy.vector.2[generation])

    tracked.response = perform_micro_mosaics_truncation(trait.mean.1 = sim.array["intervention",1, generation-1],
                                                        trait.mean.2 = sim.array["intervention", 2, generation-1],
                                                        standard.deviation = standard.deviation,
                                                        vector.length = vector.length,
                                                        n.cycles = max.cycles,
                                                        female.exposure = female.insecticide.exposure,
                                                        coverage.1 = intervention.coverage.1,
                                                        coverage.2 = intervention.coverage.1,
                                                        male.selection.differential.1 = male.selection.differentials[[1]],
                                                        male.selection.differential.2 = male.selection.differentials[[2]],
                                                        heritability = heritability,
                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                        regression.coefficient = regression.coefficient,
                                                        regression.intercept = regression.intercept,
                                                        current.insecticide.efficacy.1 = insecticide.efficacy.vector.1[generation],
                                                        current.insecticide.efficacy.2 = insecticide.efficacy.vector.2[generation],
                                                        cross.selection = cross.selection)

    sim.array['intervention', 1, generation] = tracked.response[[1]] + sim.array['intervention', 1, generation-1]
    sim.array['intervention', 2, generation] = tracked.response[[2]] + sim.array['intervention', 2, generation-1]

    sim.array['intervention', 1, generation] = ifelse(sim.array['intervention', 1, generation] < 0,
                                                      yes = 0,
                                                      no = sim.array['intervention', 1, generation])
    sim.array['intervention', 2, generation] = ifelse(sim.array['intervention', 2, generation] < 0,
                                                      yes = 0,
                                                      no = sim.array['intervention', 2, generation])


  }

  return(list(sim.array, insecticide.efficacy.vector.1, insecticide.efficacy.vector.2))
}




