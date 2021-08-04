#'@title A wrapper function for calculating the relative total population size
#'
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param standard.deviaton = The standard deviation of the trait mean in the population.


wrapper_total_population_size = function(standard.deviation,
                                         vector.length){


  relative.contributions.before.selection =  calculate_density_of_trait_values(vector.length = vector.length,
                                                                               trait.mean = 0, #exact value does not matter for calculating population size
                                                                               standard.deviation = standard.deviation)


  total.population.size = get_total_population_size_truncation(relative.contributions.before.selection = relative.contributions.before.selection)

  return(total.population.size)
}
