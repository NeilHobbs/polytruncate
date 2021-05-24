#'@title Calculate the overall selection differential as a result of both insecticide selection and fitness costs
#'
#'@param exposure.scaling.factor = A factor which converts the insecticide exposure to the selection differential.
#'@param insecticide.selection.differential = The selection differential as a result of insecticide selection.
#'@param fitness.selection.differential = The selection differential as a result of fitness costs.


calculate_insecticide_fitness_selection_differential = function(exposure.scaling.factor,
                                                                insecticide.selection.differential,
                                                                fitness.selection.differential){

  insecticide.fitness.selection.differential = (exposure.scaling.factor*
                                                  insecticide.selection.differential) +
                                                fitness.selection.differential

  return(insecticide.fitness.selection.differential)
}
