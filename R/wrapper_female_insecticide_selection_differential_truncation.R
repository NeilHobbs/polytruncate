


wrapper_female_insecticide_selection_differential_truncation = function(vector.length,
                                                                        standard.deviation,
                                                                        female.insecticide.exposure,
                                                                        female.trait.mean,
                                                                        maximum.bioassay.survival.proportion,
                                                                        michaelis.menten.slope,
                                                                        half.population.bioassay.survival.resistance,
                                                                        regression.coefficient,
                                                                        regression.intercept,
                                                                        current.insecticide.efficacy){


  #calculate the field survival for the insecticide:

  field.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                 michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                 trait.mean = female.trait.mean,
                                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                               regression.coefficient = regression.coefficient,
                                                               regression.intercept = regression.intercept,
                                                               current.insecticide.efficacy = current.insecticide.efficacy)



  ####Now do the selection differential

  #step 1: calculate the total female population size before selection
  total.female.population.size = calculate_total_female_population_size_truncation(total.population.size = wrapper_total_population_size(standard.deviation = standard.deviation,
                                                                                                                                         vector.length = vector.length))
  #step 2: Calculate the unexposed population size
  female.population.size.unexposed = calculate_female_population_size_unexposed_truncation(total.female.population.size = total.female.population.size,
                                                                                           female.insecticide.exposure = female.insecticide.exposure)

  #Step 3: calculate the population size of the exposed survivors:
  female.population.size.exposed.survivors = calculate_female_population_size_exposed_survivors_truncation(female.insecticide.exposure = female.insecticide.exposure,
                                                                                                           total.female.population.size = total.female.population.size,
                                                                                                           field.survival = field.survival)
  #Step 4: Calculate the female exposed selection differential:
  female.exposed.selection.differential = calculate_female_exposed_selection_differential_truncation(field.survival = field.survival,
                                                                                                     standard.deviation = standard.deviation)

  #Step 5: Calculate the trait mean of the exposed survivors:

  female.trait.mean.exposed.survivors = calculate_female_trait_mean_exposed_survivors_truncation(female.exposed.selection.differential = female.exposed.selection.differential,
                                                                                                 female.trait.mean = female.trait.mean)

  #Step 6: calculate the population size of unexposed and surviving individuals
  female.population.size.after.selection = calculate_female_population_size_after_selection_truncation(female.population.size.unexposed = female.population.size.unexposed,
                                                                                                       female.population.size.exposed.survivors = female.population.size.exposed.survivors)

  #Step 7: calculate the trait mean after selection has occurred:
  female.trait.mean.after.selection = calculate_female_trait_mean_after_selection_truncation(female.population.size.exposed.survivors = female.population.size.exposed.survivors,
                                                                                             female.trait.mean.exposed.survivors = female.trait.mean.exposed.survivors,
                                                                                             female.trait.mean = female.trait.mean,
                                                                                             female.population.size.after.selection = female.population.size.after.selection,
                                                                                             female.population.size.unexposed = female.population.size.unexposed)

  #Step 8: Calcualte the selection differential due to insecticide selection pressure:
  female.insecticide.selection.differential = calculate_female_insecticide_selection_differential_truncation(female.trait.mean.after.selection = female.trait.mean.after.selection,
                                                                                                             female.trait.mean = female.trait.mean)

  return(female.insecticide.selection.differential)
}
