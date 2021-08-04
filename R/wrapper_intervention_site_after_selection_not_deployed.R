#'@title A wrapper function to calculate the trait mean in the intervention site after fitness cost based selection
#'
#'@param heritabililty = The heritability of a polygenic trait.
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males

wrapper_intervention_site_after_selection_not_deployed = function(heritability,
                                                                  intervention.before.selection,
                                                                  female.fitness.cost,
                                                                  male.fitness.cost){

  response.fitness = wrapper_breeders_equation_male_female_fitness(heritability = heritability,
                                                                   trait.mean = intervention.before.selection,
                                                                   female.fitness.cost = female.fitness.cost,
                                                                   male.fitness.cost = male.fitness.cost)

  intervention.after.selection = calculate_intervention_site_after_selection_not_deployed(intervention.before.selection = intervention.before.selection,
                                                                                          response.fitness = response.fitness)

  intervention.after.selection = ifelse(intervention.after.selection < 0,
                                        yes = 0,
                                        no = intervention.after.selection)

  return(intervention.after.selection)
}
