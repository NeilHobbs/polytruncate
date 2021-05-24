#'@title Calculate the relative population contribution to the population of mosquitoes with a Polygenic Resistance Score prior to selection.
#'
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviaton = The standard deviation of the trait mean in the population.

calculate_density_of_trait_values = function(vector.length,
                                             trait.mean,
                                             standard.deviation){

  normal.distribution.vector =  create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean,
                                                           standard.deviation = standard.deviation)

relative.contribution.before.selection = stats::dnorm(x = normal.distribution.vector,
                                                      mean = trait.mean,
                                                      sd = standard.deviation,
                                                      log=FALSE)
    return(relative.contribution.before.selection)
}

