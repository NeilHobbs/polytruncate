calculate_total_response_multiple_selection_truncation = function(response.values,
                                                                  female.population.size.values){

  total.number.of.ovipositions = sum(female.population.size.values)

  #weight the responses:
  weighted.response = response.values*(female.population.size.values)/total.number.of.ovipositions

  overall.response = sum(weighted.response)

  return(overall.response)
}
