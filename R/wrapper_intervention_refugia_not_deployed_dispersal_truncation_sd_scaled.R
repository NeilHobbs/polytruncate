#'@title Wrapper to allow mosquito dispersal tracking in the intervention site when the insecticide is not deployed
#'
#'@param heritabililty = The heritability of a polygenic trait.
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males
#'@param refugia.before.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia  before selection has occurred that generation.
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.



wrapper_intervention_refugia_not_deployed_dispersal_truncation_sd_scaled = function(insecticide.population.suppression,
                                                                          intervention.before.selection,
                                                                          female.fitness.cost,
                                                                          male.fitness.cost,
                                                                          heritability,
                                                                          refugia.before.selection,
                                                                          dispersal.rate,
                                                                          intervention.coverage,
                                                                          cross.selection.matrix,
                                                                          currently.deployed.insecticide,
                                                                          currently.tracked.insecticide,
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
                                                                          intervention.before.selection.other
){


  genetic.correlation = cross.selection.matrix[currently.deployed.insecticide, currently.tracked.insecticide]

  calc.standard.devation = sd_changes_with_z(current.z = intervention.before.selection.other,
                                             z.sd.intercept = z.sd.intercept,
                                             z.sd.coefficient = z.sd.coefficient)

  indirect.cross.selection = indirect_cross_selection_truncation(genetic.correlation = genetic.correlation,
                                                                 intervention.before.selection = intervention.before.selection.other,
                                                                 female.fitness.cost = female.fitness.cost,
                                                                 male.fitness.cost = male.fitness.cost,
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
                                                                 heritability = heritability)



  intervention.after.selection = wrapper_intervention_site_after_selection_not_deployed(heritability = heritability,
                                                                                        intervention.before.selection = intervention.before.selection,
                                                                                        female.fitness.cost = female.fitness.cost,
                                                                                        male.fitness.cost = male.fitness.cost) + indirect.cross.selection





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
