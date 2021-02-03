response_fitness_cost_selection = function(fitness.selection.differential,
                                             heritability){

  response = heritability * fitness.selection.differential

  return(response)

}
