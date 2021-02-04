perform_intervention_tracking_insecticide_absent = function(dispersal.rate,
                                                             intervention.coverage,
                                                             female.insecticide.exposure,
                                                             male.insecticide.exposure,
                                                             current.intervention.intensity,
                                                             current.refugia.intensity,
                                                             sd.population.resistance,
                                                             conversion.factor,
                                                             intercept,
                                                             current.insecticide.efficacy,
                                                             insecticide.scaling.factor,
                                                             half.population.bioassay.survival.resistance,
                                                             initial.applied.efficacy,
                                                             generations.since.deployment,
                                                             basal.decay.rate,
                                                             rapid.decay.rate,
                                                             cut.off.generations){

  #Obtain the current efficacy of the insecticide in deployment
  efficacy.of.insecticide = calculate_current_insecticide_efficacy(initial.applied.efficacy =initial.applied.efficacy,
                                                                   generations.since.deployment = generations.since.deployment,
                                                                   basal.decay.rate = basal.decay.rate,
                                                                   rapid.decay.rate = rapid.decay.rate,
                                                                   cut.off.generations = cut.off.generations)

  #Calculate the selection differentials:
  refugia.fitness.differential = calculate_fitness_selection_differential(sd.population.resistance = sd.population.resistance,
                                                                          relative.fitness = relative.fitness)

  intervention.fitness.differential = calculate_fitness_selection_differential(sd.population.resistance = sd.population.resistance,
                                                                               relative.fitness = relative.fitness)

  #calculate responses to selection
  refugia.response = response_fitness_cost_selection(fitness.selection.differential = refugia.fitness.differential,
                                                     heritability = heritability)

  intervention.response = response_fitness_cost_selection(fitness.selection.differential = intervention.fitness.differential,
                                                          heritability = heritability)

  #Update the intensities after insecticide and fitness selection and response has occurred
  refugia.post.selection = track_resistance_no_insecticide_response(current.resistance.intensity,
                                                                    fitness.selection)


  intervention.post.selection = track_resistance_insecticide_response(current.resistance.intensity = ,
                                                                      response.to.selection = intervention.response)

  #Calculate the Relative Numbers to Disperse
  mosquitoes.leaving.refugia = number_migrating_refugia_to_intervention(intervention.coverage = intervention.coverage,
                                                                        dispersal.rate = dispersal.rate)

  mosquitoes.leaving.intervention = number_migrating_intervention_to_refugia(dispersal.rate = dispersal.rate,
                                                                             intervention.coverage = intervention.coverage,
                                                                             female.insecticide.exposure = female.insecticide.exposure,
                                                                             male.insecticide.exposure = male.insecticide.exposure,
                                                                             current.resistance.intensity = current.intervention.intensity,
                                                                             sd.population.resistance = sd.population.resistance,
                                                                             conversion.factor = conversion.factor,
                                                                             intercept = intercept,
                                                                             current.insecticide.efficacy = efficacy.of.insecticide)

  #Then perform the mosquito dispersal
  next.generation.intensity = track_intervention_site_after_migration(number.intervention.to.refugia = mosquitoes.leaving.intervention,
                                                                      number.refugia.to.intervention = mosquitoes.leaving.refugia,
                                                                      tracked.intervention.intensity = intervention.post.selection,
                                                                      tracked.refugia.intensity = refugia.post.selection)

  return(next.generation.intensity)
}
