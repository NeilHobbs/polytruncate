#' @title
#'

#implements equation 2e from base model v2

calculate_resistance_intensity_exposed_survivors = function(current.resistance.intensity,
                                                            half.population.bioassay.survival.resistance = 900,
                                                            conversion.factor = 0.48,
                                                            intercept = 0.15,
                                                            current.insecticide.efficacy){

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

  insecticide.selection.differential = calculate_selection_differential_insecticide_exposed(sd.population.resistance = sd.population.resistance,
                                                                                            field.survival = field.survival.proportion)



  resistance.intensity.exposed.survivors = insecticide.selection.differential + current.resistance.intensity

  return(resistance.intensity.exposed.survivors)
}
