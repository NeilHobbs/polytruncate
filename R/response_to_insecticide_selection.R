

response_to_insecticide_selection = function(insecticide.selection.differential,
                                             fitness.selection.differential,
                                             heritability,
                                             insecticide.scaling.factor){


  response = heritability * ((insecticide.scaling.factor*insecticide.selection.differential) + fitness.selection.differential)

  return(response)

}
