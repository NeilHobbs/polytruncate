#'@title Calculate the relative population size of male mosquitoes that have encountered the insecticide and survived.
#'
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param total.male.population.size = The total relative population size of male mosquitoes in the intervention site before insecticide selection.
#'@param field.survival = The survival probability in the field to insecticide i.
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the insecticide


calculate_male_population_size_exposed_survivors_truncation = function(male.insecticide.exposure,
                                                                       female.insecticide.exposure,
                                                                         total.male.population.size,
                                                                         field.survival){

  male.population.size.exposed.survivors = (male.insecticide.exposure * female.insecticide.exposure) * total.male.population.size * field.survival

  return(male.population.size.exposed.survivors)
}
