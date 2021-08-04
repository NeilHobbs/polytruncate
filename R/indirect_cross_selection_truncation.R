


indirect_cross_selection_truncation = function(genetic.correlation,
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
                                               heritability){

  direct.selection  =  wrapper_intervention_site_after_truncation_selection_deployed(intervention.before.selection = intervention.before.selection,
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
  indirect.selection = direct.selection * genetic.correlation

  return(indirect.selection)

}
