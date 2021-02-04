

#Eq 6b:

number_migrating_refugia_to_intervention = function(intervention.coverage,
                                             dispersal.rate){

  count.mosquitoes = (1 - intervention.coverage)*dispersal.rate

  return(count.mosquitoes)
}
