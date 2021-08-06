#'@title Wrapper to allow mosquito dispersal tracking in the intervention site when the insecticide is deployed
#'
#'@param insecticide.population.suppression = The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.
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
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.
#'@param refugia.before.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia  before selection has occurred that generation.


wrapper_intervention_refugia_deployed_dispersal_truncation = function(insecticide.population.suppression,
                                                           intervention.before.selection,
                                                           female.fitness.cost,
                                                           male.fitness.cost,
                                                           female.insecticide.exposure,
                                                           male.insecticide.exposure,
                                                           standard.deviation,
                                                           vector.length,
                                                           maximum.bioassay.survival.proportion,
                                                           michaelis.menten.slope,
                                                           half.population.bioassay.survival.resistance,
                                                           regression.coefficient,
                                                           regression.intercept,
                                                           current.insecticide.efficacy,
                                                           exposure.scaling.factor,
                                                           heritability,
                                                           refugia.before.selection,
                                                           dispersal.rate,
                                                           intervention.coverage){

  intervention.after.selection = wrapper_intervention_site_after_truncation_selection_deployed(intervention.before.selection = intervention.before.selection,
                                                                                    female.fitness.cost = female.fitness.cost,
                                                                                    male.fitness.cost = male.fitness.cost,
                                                                                    female.insecticide.exposure = female.insecticide.exposure,
                                                                                    male.insecticide.exposure = male.insecticide.exposure,
                                                                                    standard.deviation = standard.deviation,
                                                                                    vector.length = vector.length,
                                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                    regression.coefficient = regression.coefficient,
                                                                                    regression.intercept = regression.intercept,
                                                                                    current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                                    heritability = heritability)


  refugia.after.selection = wrapper_refugia_breeders_equation(refugia.before.selection = refugia.before.selection,
                                                              heritability = heritability,
                                                              female.fitness.cost = female.fitness.cost,
                                                              male.fitness.cost = male.fitness.cost)

  staying.in.intervention = number_migrating_intervention_to_refugia(dispersal.rate = 1 - dispersal.rate,
                                                                         intervention.coverage = intervention.coverage)

  joining.from.intervention = number_migrating_intervention_to_refugia(dispersal.rate = dispersal.rate,
                                                                      intervention.coverage = intervention.coverage)

  joining.from.refugia =  number_migrating_refugia_to_intervention(dispersal.rate = dispersal.rate,
                                                                   intervention.coverage = intervention.coverage)

  staying.in.refugia = number_migrating_refugia_to_intervention(dispersal.rate = 1 - dispersal.rate,
                                                                    intervention.coverage = intervention.coverage)


  intervention.after.migration = intervention_after_migration(intervention.after.selection = intervention.after.selection,
                                                              staying.in.intervention = staying.in.intervention,
                                                              insecticide.population.suppression = insecticide.population.suppression,
                                                              joining.from.refugia = joining.from.refugia,
                                                              refugia.after.selection = refugia.after.selection)


  refugia.after.migration = refugia_after_migration(intervention.after.selection = intervention.after.selection,
                                                    joining.from.intervention = joining.from.intervention,
                                                    insecticide.population.suppression = insecticide.population.suppression,
                                                    refugia.after.selection = refugia.after.selection,
                                                    staying.in.refugia = staying.in.refugia)


  #prevent from going below zero
  refugia.after.migration = ifelse(refugia.after.migration < 0,
                                   yes = 0,
                                   no = refugia.after.migration)

  intervention.after.migration = ifelse(intervention.after.migration < 0,
                                        yes = 0,
                                        no = intervention.after.migration)

  return(list(intervention.after.migration, refugia.after.migration))

}
