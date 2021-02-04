track_refugia_after_migration = function(number.intervention.to.refugia,
                                         number.refugia.to.intervention,
                                         current.intervention.intensity,
                                         current.refugia.intensity){

  mosquitoes.staying.refugia = current.refugia.intensity *(1-number.refugia.to.intervention)

  mosquitoes.joinning.from.intervention = current.intervention.intensity * number.intervention.to.refugia

  update.refugia.intensity = mosquitoes.staying.refugia + mosquitoes.joinning.from.intervention

  return(update.refugia.intensity)
}
