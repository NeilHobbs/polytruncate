#' @title Calculate the selection differential as a result of insecticidal selection pressure.
#'
#' @param current.resistance.intensity = The current resistance intensity in the location where the insecticide selection is occurring.
#' @param sd.population.resistance = The standard deviation of the resistance intensity values in the mosquito population
#' @param half.population.bioassay.survival.resistance = The resistance intensity that would give 50% survival in the bioassay.
#' @param conversion.factor = The regression coefficient to convert bioassay survival to field survival
#' @param intercept = The intercept from the linear regression to convert bioassay survival to field survival
#' @param female.insecticide.exposure = The proportion of female mosquitoes exposed to the insecticide
#' @param male.insecticide.exposure = The proportion of male mosquitoes exposed to the insecticide as a proportion of the female exposure.
#' @param current.insecticide.efficacy = The current efficacy of the insecticide in deployment.
#'
#' @return The selection differential as a result of insecticide selection pressure.

calculate_insecticide_selection_differential = function(current.resistance.intensity,
                                                        sd.population.resistance,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        conversion.factor = 0.48,
                                                        intercept = 0.15,
                                                        female.insecticide.exposure,
                                                        male.insecticide.exposure,
                                                        current.insecticide.efficacy){

  #Calculate the exposure to the insecticide
  exposure = calculate_insecticide_exposure(female.insecticide.exposure = female.insecticide.exposure,
                                            male.insecticide.exposure = male.insecticide.exposure)

  #Calculate the True bioassay proportion
  bioassay.survival.proportion = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                 mean.population.resistance = current.resistance.intensity,
                                                                 michaelis.menten.slope = 1,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 sd.population.resistance = sd.population.resistance,
                                                                 number.bioassays = 10000 #set high so as to calculate the true mean.
  )

#Convert the bioassay survival to field survival
  field.survival.proportion = convert_bioassay_survival_to_field(bioassay.survival = bioassay.survival.proportion,
                                                                 conversion.factor = conversion.factor,#values obtained from linear modelling.
                                                                 intercept = intercept,
                                                                 current.insecticide.efficacy = current.insecticide.efficacy)
#Calculate the resistance intensity of the exposured survivors
  intensity.exposed.survivors = calculate_resistance_intensity_exposed_survivors(current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                 current.resistance.intensity = current.resistance.intensity,
                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                 conversion.factor = conversion.factor,
                                                                                 intercept = intercept,
                                                                                 sd.population.resistance = sd.population.resistance)


#Population is made of two groups: those not exposed to the insecticide
  unexposed.group = (1-exposure) * current.resistance.intensity
  #and those exposed (and surviving) insecticide
  exposed.group = exposure * field.survival.proportion * intensity.exposed.survivors

  #Calculate the selection differential; weighted by the relative population sizes.
  insecticide.selection.differential = (unexposed.group + exposed.group)/((1-exposure) + exposure*field.survival.proportion)

  return(insecticide.selection.differential)
}




