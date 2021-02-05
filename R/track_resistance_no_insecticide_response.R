track_resistance_no_insecticide_response = function(current.resistance.intensity,
                                                 fitness.response){


  update.resistance.intensity = current.resistance.intensity + fitness.response

  #prevent resistance intensity going below zero
  update.resistance.intensity = ifelse(update.resistance.intensity < 0, yes = 0, no = update.resistance.intensity)

  return(update.resistance.intensity)

}
