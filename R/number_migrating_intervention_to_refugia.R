number_migrating_intervention_to_refugia = function(dispersal.rate,
                                                    intervention.coverage,
                                                    female.insecticide.exposure,
                                                    male.insecticide.exposure,
                                                    current.resistance.intensity,
                                                    sd.population.resistance,
                                                    conversion.factor,
                                                    intercept,
                                                    current.insecticide.efficacy){

  bioassay.survival.proportion = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                 mean.population.resistance = 900,
                                                                 michaelis.menten.slope = 1,
                                                                 half.population.bioassay.survival.resistance = current.resistance.intensity,
                                                                 sd.population.resistance = sd.population.resistance,
                                                                 number.bioassays = 10000 #set high to get true mean
  )

  field.survival.proportion = convert_bioassay_survival_to_field(bioassay.survival = bioassay.survival.proportion,
                                                                 conversion.factor = conversion.factor,#values obtained from linear modelling.
                                                                 intercept = intercept,
                                                                 current.insecticide.efficacy = current.insecticide.efficacy)

  exposure = calculate_insecticide_exposure(female.insecticide.expsure = female.insecticide.expsure,
                                            male.insecticide.exposure = male.insecticide.exposure)

  impact.of.intervention = (1 - exposure) + (exposure*field.survival.proportion)

  number.of.mosquitoes = dispersal.rate * intervention.coverage * impact.of.intervention

return(numnumber.of.mosquitoes)
}
