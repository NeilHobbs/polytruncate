track_intervention_site_after_migration = function(number.intervention.to.refugia,
                                                   number.refugia.to.intervention,
                                                   tracked.intervention.intensity,
                                                   tracked.refugia.intensity){

mosquitoes.staying.intervention = tracked.intervention.intensity * (1-number.intervention.to.refugia)
mosquitoes.joinning.from.refugia = tracked.refugia.intensity * number.refugia.to.intervention

update.intervention.intensity = (mosquitoes.staying.intervention + mosquitoes.joinning.from.refugia)/((1-number.intervention.to.refugia) + number.refugia.to.intervention)

return(update.intervention.intensity)
}
