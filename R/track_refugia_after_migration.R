track_refugia_after_migration = function(number.intervention.to.refugia,
                                         number.refugia.to.intervention,
                                         tracked.intervention.intensity,
                                         tracked.refugia.intensity){

  mosquitoes.staying.refugia = tracked.refugia.intensity *(1-number.refugia.to.intervention)

  mosquitoes.joinning.from.intervention = tracked.intervention.intensity * number.intervention.to.refugia

  update.refugia.intensity = mosquitoes.staying.refugia + mosquitoes.joinning.from.intervention/((1-number.refugia.to.intervention) + number.intervention.to.refugia)

  return(update.refugia.intensity)
}
