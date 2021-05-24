#'@title Calculate the response to selection from fitness costs only.
#'
#'@description Calculates the response to selection from fitness costs only, such as would occur when the
#'insecticide is either not deployed in the intervention site or the selection that occurs
#'in the refugia.
#'
#'@param male.fitness.selection.differential = The selection differential for male mosquitoes from fitness costs
#'@param female.fitness.selection.differential = The selection differential for female mosquitoes from fitness costs
#'@param heritability = The heritability of a polygenic trait.


breeders_equation_male_female_fitness = function(male.fitness.selection.differential,
                                                 female.fitness.selection.differential,
                                                 heritability){

  if(0 > heritability |heritability > 1){stop("heritability must be between 0 and 1")}

  response.fitness = ((heritability/2)* female.fitness.selection.differential)+
                     ((heritability/2)* male.fitness.selection.differential)


  return(response.fitness)
}
