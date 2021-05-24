#'@title Calculate the selection differential for female mosquitoes following insecticide selection and fitness costs.
#'
#'@param female.insecticide.selection.differential = The selection differential for female mosquitoes following insecticide selection.
#'@param exposure.scaling.factor = A factor which converts the insecticide exposure to the selection differential.
#'@param female.fitness.selection.differential = The selection differential for female mosquitoes from fitness costs


calculate_female_insecticide_fitness_selection_differential = function(female.insecticide.selection.differential,
                                                                       exposure.scaling.factor,
                                                                       female.fitness.selection.differential){

  female.insecticide.fitness.selection.differential = (female.insecticide.selection.differential *
                                                         exposure.scaling.factor)+
                                                       female.fitness.selection.differential

  return(female.insecticide.fitness.selection.differential)
}
