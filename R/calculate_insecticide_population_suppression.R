#'@title The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.
#'
#'@param female.population.size.after.selection = The relative population size of all the female mosquitoes in the intervention site after insecticide selection.
#'@param total.female.population.size = The total relative population size of female mosquitoes in the intervention site before insecticide selection.


calculate_insecticide_population_suppression = function(female.population.size.after.selection,
                                                        total.female.population.size){

  insecticide.population.suppression = (female.population.size.after.selection/total.female.population.size)

    return(insecticide.population.suppression)

}
