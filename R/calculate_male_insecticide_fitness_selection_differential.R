#'@title Calculate the selection differential for male mosquitoes following insecticide selection and fitness costs.
#'
#'@param male.insecticide.selection.differential = The selection differential for male mosquitoes following insecticide selection.
#'@param exposure.scaling.factor = A factor which converts the insecticide exposure to the selection differential.
#'@param male.fitness.selection.differential = The selection differential for male mosquitoes from fitness costs


calculate_male_insecticide_fitness_selection_differential = function(male.insecticide.selection.differential,
                                                                       exposure.scaling.factor,
                                                                       male.fitness.selection.differential){

  male.insecticide.fitness.selection.differential = (male.insecticide.selection.differential *
                                                         exposure.scaling.factor)+
    male.fitness.selection.differential

  return(male.insecticide.fitness.selection.differential)
}
