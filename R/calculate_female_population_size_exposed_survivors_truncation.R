#'@title Calculate the relative population size of female mosquitoes that have encountered the insecticide and survived.
#'
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param total.female.population.size = The total relative population size of female mosquitoes in the intervention site before insecticide selection.
#'@param field.survival = The survival probability in the field to insecticide i.


calculate_female_population_size_exposed_survivors_truncation = function(female.insecticide.exposure,
                                                                         total.female.population.size,
                                                                         field.survival){

  female.population.size.exposed.survivors = female.insecticide.exposure * total.female.population.size * field.survival

  return(female.population.size.exposed.survivors)
}
