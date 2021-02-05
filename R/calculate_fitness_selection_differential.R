#' @title Calculate the selection differential caused by differences in the relative fitness of individuals in the population
#'
#' @param sd.population.resistance = The standard deviation of the resistance intensity values in the mosquito population
#' @param relative.fitness = The relative fitness of the most resistant individuals in the population compared to the least resistant individuals in the population.

calculate_fitness_selection_differential = function(sd.population.resistance,
                                                    relative.fitness){

  if(0 > relative.fitness){stop("relative.fitness must be greater than 0")}

  fitness.selection.differential =  -sd.population.resistance * (dnorm(pnorm(1-relative.fitness))/relative.fitness)

  return(fitness.selection.differential)

}


