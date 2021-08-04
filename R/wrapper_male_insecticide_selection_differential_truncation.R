#'@title A wrapper function for calculating the insecticide selection differential in male mosquitoes
#'
#'@param vector.length =
#'@param standard.deviation =
#'@param male.insecticide.exposure =
#'@param female.insecticide.exposure =
#'@param male.trait.mean =
#'@param maximum.bioassay.survival.proportion =
#'@param michaelis.menten.slope =
#'@param half.population.bioassay.survival.resistance =
#'@param regression.coefficient =
#'@param regression.intercept =
#'@param current.insecticide.efficacy =

wrapper_male_insecticide_selection_differential_truncation = function(vector.length,
                                                                      standard.deviation,
                                                                      male.insecticide.exposure,
                                                                      female.insecticide.exposure,
                                                                      male.trait.mean,
                                                                      maximum.bioassay.survival.proportion,
                                                                      michaelis.menten.slope,
                                                                      half.population.bioassay.survival.resistance,
                                                                      regression.coefficient,
                                                                      regression.intercept,
                                                                      current.insecticide.efficacy){


  #calculate the field survival for the insecticide:

  field.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                 michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                 trait.mean = male.trait.mean,
                                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                               regression.coefficient = regression.coefficient,
                                                               regression.intercept = regression.intercept,
                                                               current.insecticide.efficacy = current.insecticide.efficacy)



  ####Now do the selection differential

  #step 1: calculate the total male population size before selection
  total.male.population.size = calculate_total_male_population_size_truncation(total.population.size = wrapper_total_population_size(standard.deviation = standard.deviation,
                                                                                                                                     vector.length = vector.length))
  #step 2: Calculate the unexposed population size
  male.population.size.unexposed = calculate_male_population_size_unexposed_truncation(total.male.population.size = total.male.population.size,
                                                                                       male.insecticide.exposure = male.insecticide.exposure,
                                                                                       female.insecticide.exposure = female.insecticide.exposure)

  #Step 3: calculate the population size of the exposed survivors:
  male.population.size.exposed.survivors = calculate_male_population_size_exposed_survivors_truncation(male.insecticide.exposure = male.insecticide.exposure,
                                                                                                       female.insecticide.exposure = female.insecticide.exposure,
                                                                                                       total.male.population.size = total.male.population.size,
                                                                                                       field.survival = field.survival)
  #Step 4: Calculate the male exposed selection differential:
  male.exposed.selection.differential = calculate_male_exposed_selection_differential_truncation(field.survival = field.survival,
                                                                                                 standard.deviation = standard.deviation)

  #Step 5: Calculate the trait mean of the exposed survivors:

  male.trait.mean.exposed.survivors = calculate_male_trait_mean_exposed_survivors_truncation(male.exposed.selection.differential = male.exposed.selection.differential,
                                                                                             male.trait.mean = male.trait.mean)

  #Step 6: calculate the population size of unexposed and surviving individuals
  male.population.size.after.selection = calculate_male_population_size_after_selection_truncation(male.population.size.unexposed = male.population.size.unexposed,
                                                                                                   male.population.size.exposed.survivors = male.population.size.exposed.survivors)

  #Step 7: calculate the trait mean after selection has occurred:
  male.trait.mean.after.selection = calculate_male_trait_mean_after_selection_truncation(male.population.size.exposed.survivors = male.population.size.exposed.survivors,
                                                                                         male.trait.mean.exposed.survivors = male.trait.mean.exposed.survivors,
                                                                                         male.trait.mean = male.trait.mean,
                                                                                         male.population.size.after.selection = male.population.size.after.selection,
                                                                                         male.population.size.unexposed = male.population.size.unexposed)

  #Step 8: Calcualte the selection differential due to insecticide selection pressure:
  male.insecticide.selection.differential = calculate_male_insecticide_selection_differential_truncation(male.trait.mean.after.selection = male.trait.mean.after.selection,
                                                                                                         male.trait.mean = male.trait.mean)

  return(male.insecticide.selection.differential)
}
