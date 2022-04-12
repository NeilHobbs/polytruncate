#'@title Wrapper function to get the Mean Polygenic Resistance Score  in the Intervention Site after Selection
#'
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness cost associated with polygenic resistance for males.
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.,
#'@param exposure.scaling.factor = A factor which converts the insecticide exposure to the selection differential.
#'@param heritability = The heritability of a polygenic trait.



wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled = function(intervention.before.selection,
                                                                                  female.fitness.cost,
                                                                                  male.fitness.cost,
                                                                                  female.insecticide.exposure,
                                                                                  male.insecticide.exposure,
                                                                                  z.sd.intercept,
                                                                                  z.sd.coefficient,
                                                                                  vector.length,
                                                                                  maximum.bioassay.survival.proportion,
                                                                                  michaelis.menten.slope,
                                                                                  half.population.bioassay.survival.resistance,
                                                                                  regression.coefficient,
                                                                                  regression.intercept,
                                                                                  current.insecticide.efficacy,
                                                                                  exposure.scaling.factor,
                                                                                  heritability,
                                                                                  survival.to.other.insecticide){

  calc.standard.devation = sd_changes_with_z(current.z = intervention.before.selection,
                                             z.sd.intercept = z.sd.intercept,
                                             z.sd.coefficient = z.sd.coefficient)

  #Calculate insecticide fitness response in Intervention Site
  response.insecticide.fitness = wrapper_breeders_equation_insecticide_fitness_truncation_mixtures(trait.mean = intervention.before.selection,
                                                                                                   female.fitness.cost = female.fitness.cost,
                                                                                                   male.fitness.cost= male.fitness.cost,
                                                                                                   female.insecticide.exposure = female.insecticide.exposure,
                                                                                                   male.insecticide.exposure = male.insecticide.exposure,
                                                                                                   standard.deviation = calc.standard.devation,
                                                                                                   vector.length = vector.length,
                                                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                   regression.coefficient = regression.coefficient,
                                                                                                   regression.intercept = regression.intercept,
                                                                                                   current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                                   exposure.scaling.factor = exposure.scaling.factor,
                                                                                                   heritability = heritability,
                                                                                                   survival.to.other.insecticide = survival.to.other.insecticide)


  intervention.after.selection = calculate_intervention_site_after_truncation_selection_deployed(intervention.before.selection = intervention.before.selection,
                                                                                                 response.insecticide.fitness = response.insecticide.fitness)

  return(intervention.after.selection)

}
