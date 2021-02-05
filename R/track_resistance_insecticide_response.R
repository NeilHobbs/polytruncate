
track_resistance_insecticide_response = function(current.resistance.intensity,
                                                 response.to.selection){


  update.resistance.intensity = current.resistance.intensity + response.to.selection

  #prevent resistance intensity going below zero
  update.resistance.intensity = ifelse(update.resistance.intensity < 0, yes = 0, no = update.resistance.intensity)

  return(update.resistance.intensity)

}
