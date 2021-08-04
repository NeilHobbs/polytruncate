#'@title Wrapper to perfrom the Breeders equation in the Refugia
#'
#'@param heritability = The heritability of a polygenic trait.
#'@param refugia.before.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia  before selection has occurred that generation.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males'

wrapper_refugia_breeders_equation = function(refugia.before.selection,
                                             heritability,
                                             female.fitness.cost,
                                             male.fitness.cost){



  refugia.after.selection = refugia.before.selection +  wrapper_breeders_equation_male_female_fitness(heritability = heritability,
                                                                                                      trait.mean = refugia.before.selection,
                                                                                                      female.fitness.cost = female.fitness.cost,
                                                                                                      male.fitness.cost = male.fitness.cost)
  refugia.after.selection = ifelse(refugia.after.selection < 0,
                                   yes = 0,
                                   no = refugia.after.selection)

  return(refugia.after.selection)
}
