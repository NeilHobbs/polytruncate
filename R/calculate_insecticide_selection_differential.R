calculate_insecticide_selection_differential = function(current.resistance.intensity,
                                                        sd.population.resistance,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        conversion.factor = 0.48,
                                                        intercept = 0.15,
                                                        female.insecticide.exposure,
                                                        male.insecticide.exposure,
                                                        current.insecticide.efficacy){

  exposure = calculate_insecticide_exposure(female.insecticide.exposure = female.insecticide.exposure,
                                            male.insecticide.exposure = male.insecticide.exposure)

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
                                                                 current.insecticide.efficacy = current.insecticide.efficacy)

  intensity.exposed.survivors = calculate_resistance_intensity_exposed_survivors(current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                 current.resistance.intensity = current.resistance.intensity,
                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                 conversion.factor = conversion.factor,
                                                                                 intercept = intercept)



  unexposed.group = (1-exposure) * current.resistance.intensity
  exposed.group = exposure * field.survival.proportion * intensity.exposed.survivors

  insecticide.selection.differential = (unexposed.group + exposed.group)/((1-exposure) + exposure*field.survivors.proportion)

  return(insecticide.selection.differential)
}




