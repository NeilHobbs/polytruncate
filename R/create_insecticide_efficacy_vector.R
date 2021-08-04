#'@title Create a vector which contains the insecticide efficacy values
#'
#'@param applied.insecticide.dose = the actual deployed insecticide dose
#'@param recommended.insecticide.dose = the manufacturer's recommended dose for deployment
#'@param threshold.generations = the number of generations until the insecticide switches to decaying rapidly
#'@param base.efficacy.decay.rate = the base decay rate of the insecticide
#'@param rapid.decay.rate = the rapid decay reate of the insecticide
#'@param deployment.frequency = The number of generations between each insecticide deployment decision


create_insecticide_efficacy_vector = function(applied.insecticide.dose,
                                              recommended.insecticide.dose,
                                              threshold.generations,
                                              base.efficacy.decay.rate,
                                              rapid.decay.rate,
                                              deployment.frequency){

  start.efficacy = calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose,
                                                                             recommended.insecticide.dose)
  gens.vector = seq(1, deployment.frequency, by = 1)
  efficacy.vector = c()

  for(g in 1:deployment.frequency){


    efficacy.vector[g] = calculate_current_insecticide_efficacy(generations.since.deployment = gens.vector[g],
                                                                threshold.generations = threshold.generations,
                                                                initial.insecticide.efficacy = start.efficacy,
                                                                base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                rapid.decay.rate = rapid.decay.rate)
  }
  efficacy.vector.updated = c(start.efficacy, efficacy.vector)

  return(efficacy.vector.updated[1:deployment.frequency])

}
