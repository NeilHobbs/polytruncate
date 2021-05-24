#'@title Calculate the population size of the exposed survivors
#'
#'@param insecticide.exposure = The proportion of the mosquito population in the intervention site being exposed to an insecticide.
#'@param total.population.size = The relative population size of the intervention site prior to selection.
#'@param field.survival = The survival probability in the field to insecticide


calculate_population_size_exposed_survivors_truncation = function(insecticide.exposure,
                                                                  total.population.size,
                                                                  field.survival){

  population.size.exposed.survivors = insecticide.exposure * total.population.size * field.survival

  return(population.size.exposed.survivors)

}
