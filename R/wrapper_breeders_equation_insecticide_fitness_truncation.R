wrapper_breeders_equation_insecticide_fitness_truncation = function(vector.length,
                                                                    standard.deviation,
                                                                    male.insecticide.exposure,
                                                                    female.insecticide.exposure,
                                                                    trait.mean,
                                                                    maximum.bioassay.survival.proportion,
                                                                    michaelis.menten.slope,
                                                                    half.population.bioassay.survival.resistance,
                                                                    regression.coefficient,
                                                                    regression.intercept,
                                                                    current.insecticide.efficacy,
                                                                    exposure.scaling.factor,
                                                                    heritability,
                                                                    male.fitness.cost,
                                                                    female.fitness.cost){

  #female selection differentials:
  #Insecticide by truncation
  female.insecticide.selection.differential =  wrapper_female_insecticide_selection_differential_truncation(vector.length = vector.length,
                                                                                                            standard.deviation = standard.deviation,
                                                                                                            female.insecticide.exposure = female.insecticide.exposure,
                                                                                                            female.trait.mean = trait.mean,
                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                            regression.coefficient = regression.coefficient,
                                                                                                            regression.intercept = regression.intercept,
                                                                                                            current.insecticide.efficacy = current.insecticide.efficacy)


  #fitness costs
  female.fitness.selection.differential = female.fitness.cost

  #Total female selection differential
  female.insecticide.fitness.selection.differential = calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = female.insecticide.selection.differential,
                                                                                                                exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                female.fitness.selection.differential = female.fitness.selection.differential)



  #male selection differentials
  #insecticide by truncation

  male.insecticide.selection.differential = wrapper_male_insecticide_selection_differential_truncation(vector.length = vector.length,
                                                                                                       standard.deviation = standard.deviation,
                                                                                                       male.insecticide.exposure = male.insecticide.exposure,
                                                                                                       female.insecticide.exposure = female.insecticide.exposure,
                                                                                                       male.trait.mean = trait.mean,
                                                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                       regression.coefficient = regression.coefficient,
                                                                                                       regression.intercept = regression.intercept,
                                                                                                       current.insecticide.efficacy = current.insecticide.efficacy)

  #fitness costs
  male.fitness.selection.differential = male.fitness.cost
  #Total male selection differential
  male.insecticide.fitness.selection.differential = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.selection.differential,
                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                              male.fitness.selection.differential = male.fitness.selection.differential)

  #Breeders Equation:

  response.insecticide.fitness =  breeders_equation_male_female_insecticide_fitness(heritability = heritability,
                                                                                    male.insecticide.fitness.selection.differential = male.insecticide.fitness.selection.differential,
                                                                                    female.insecticide.fitness.selection.differential = female.insecticide.fitness.selection.differential)

  #NA becomes zero
  response.insecticide.fitness = ifelse(is.na(response.insecticide.fitness),
                                        yes = 0,
                                        no = response.insecticide.fitness)

  return(response.insecticide.fitness)
}
