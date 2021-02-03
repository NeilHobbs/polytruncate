calculate_insecticide_selection_differential = function(initial.applied.efficacy,
                                                        generations.since.deployment,
                                                        basal.decay.rate,
                                                        rapid.decay.rate,
                                                        cut.off.generations,
                                                        current.resistance.intensity,
                                                        sd.population.resistance,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        conversion.factor = 0.48,
                                                        intercept = 0.15,
                                                        female.insecticide.exposure,
                                                        male.insecticide.exposure){

  exposure = calculate_insecticide_exposure(female.insecticide.exposure = female.insecticide.exposure,
                                            male.insecticide.exposure = male.insecticide.exposure)

  insecticide.efficacy.now = calculate_current_insecticide_efficacy(initial.applied.efficacy = initial.applied.efficacy,
                                                                    generations.since.deployment = generations.since.deployment,
                                                                    basal.decay.rate = basal.decay.rate,
                                                                    rapid.decay.rate = rapid.decay.rate,
                                                                    cut.off.generations = cut.off.generations)


  bioassay.survival.proportion = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                 mean.population.resistance = current.resistance.intensity,
                                                                 michaelis.menten.slope = 1,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 sd.population.resistance = sd.population.resistance,
                                                                 number.bioassays = 10000 #set high so as to calculate the true mean.
  )


  field.survival.proportion = convert_bioassay_survival_to_field(bioassay.survival = bioassay.survival.proportion,
                                                                 conversion.factor = conversion.factor,#values obtained from linear modelling.
                                                                 intercept = intercept,
                                                                 current.insecticide.efficacy = insecticide.efficacy.now)

  intensity.exposed.survivors = calculate_resistance_intensity_exposed_survivors(initial.applied.efficacy = initial.applied.efficacy,
                                                                                 generations.since.deployment = generations.since.deployment,
                                                                                 basal.decay.rate = basal.decay.rate,
                                                                                 rapid.decay.rate = rapid.decay.rate,
                                                                                 cut.off.generations = cut.off.generations,
                                                                                 current.resistance.intensity = current.resistance.intensity,
                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                 conversion.factor = conversion.factor,
                                                                                 intercept = intercept)



  unexposed.group = (1-exposure) * current.resistance.intensity
  exposed.group = exposure * field.survival.proportion * intensity.exposed.survivors

  insecticide.selection.differential = (unexposed.group + exposed.group)/((1-exposure) + exposure*field.survivors.proportion)

  return(insecticide.selection.differential)
}




