#Note this is not giving the same response as with the formal truncation selection methodology,
#it is out in the calculation of the new PRS mean by often only a small amount but this equates to
#large errors over the course of multiple generations.

#We would expect n.cycles =  1 to perform identically to the standard truncation selection methodology.

perform_male_truncation_selection_micro_mosaics =function(trait.mean.1,
                                                          trait.mean.2,
                                                          standard.deviation,
                                                          vector.length,
                                                          female.exposure,
                                                          male.exposure,
                                                          coverage.1,
                                                          coverage.2,
                                                          heritability,
                                                          exposure.scaling.factor,
                                                          maximum.bioassay.survival.proportion,
                                                          half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope,
                                                          regression.coefficient,
                                                          regression.intercept,
                                                          current.insecticide.efficacy.1,
                                                          current.insecticide.efficacy.2){

  normal.distribution.values.1 = create_normal_distribution(vector.length = vector.length,
                                                            trait.mean = trait.mean.1,
                                                            standard.deviation = standard.deviation)
  normal.distribution.values.2 = create_normal_distribution(vector.length = vector.length,
                                                            trait.mean = trait.mean.1,
                                                            standard.deviation = standard.deviation)

  relative.trait.frequency.1 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.2,
                                                                 standard.deviation = standard.deviation)

  relative.trait.frequency.2 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.2,
                                                                 standard.deviation = standard.deviation)



  survival.rate.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                  trait.mean = trait.mean.1,
                                                                                                                                  michaelis.menten.slope = michaelis.menten.slope),
                                                                regression.coefficient = regression.coefficient,
                                                                regression.intercept = regression.intercept,
                                                                current.insecticide.efficacy = current.insecticide.efficacy.1)

  survival.rate.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                  trait.mean = trait.mean.2,
                                                                                                                                  michaelis.menten.slope = michaelis.menten.slope),
                                                                regression.coefficient = regression.coefficient,
                                                                regression.intercept = regression.intercept,
                                                                current.insecticide.efficacy = current.insecticide.efficacy.1)


  do.not.encounter = 1 - ((coverage.1 + coverage.2) * female.exposure*male.exposure)




  #Values needed for Trait 1 [assumes traits are unlinked]
  relative.frequency.do.not.encounter.1.2 = do.not.encounter*relative.trait.frequency.1
  relative.frequency.encounter.1 = female.exposure * male.exposure * coverage.1 * relative.trait.frequency.1
  relative.frequency.do.not.encounter.1.encounter.2 = female.exposure * male.exposure * coverage.2 * relative.trait.frequency.1 * survival.rate.2

  total.males.surviving.exposure.1 = (sum(relative.frequency.encounter.1)*survival.rate.1)

  cumulative.frequency.1 = cumsum(rev(c(relative.frequency.encounter.1)))

  vector.position.1 = which(abs(cumulative.frequency.1 - total.males.surviving.exposure.1) == min(abs(cumulative.frequency.1 - total.males.surviving.exposure.1)))

  #those below the threshold all die.
  post.encounter.1 = relative.frequency.encounter.1
  post.encounter.1[0:(vector.length-vector.position.1)] = 0

  update.end.relative.frequency.1 = post.encounter.1 + relative.frequency.do.not.encounter.1.2+ relative.frequency.do.not.encounter.1.encounter.2

  total.males.surviving.1 = sum(c(update.end.relative.frequency.1))

  update.mean.PRS.1 =  sum((c(update.end.relative.frequency.1) * normal.distribution.values.1))/total.males.surviving.1

  male.selection.differential.1 = update.mean.PRS.1 - trait.mean.1



  #Values needed for Trait 2
  relative.frequency.do.not.encounter.2.1 = do.not.encounter*relative.trait.frequency.2
  relative.frequency.encounter.2 = female.exposure * male.exposure* coverage.2 * relative.trait.frequency.2
  relative.frequency.do.not.encounter.2.encounter.1 = female.exposure * male.exposure* coverage.1 * relative.trait.frequency.2 * survival.rate.1

  total.males.surviving.exposure.2 = (sum(relative.frequency.encounter.2)*survival.rate.2)

  cumulative.frequency.2 = cumsum(rev(c(relative.frequency.encounter.2)))

  vector.position.2 = which(abs(cumulative.frequency.2 - total.males.surviving.exposure.2) == min(abs(cumulative.frequency.2 - total.males.surviving.exposure.2)))

  #those below the threshold all die.
  post.encounter.2 = relative.frequency.encounter.2
  post.encounter.2[0:(vector.length-vector.position.2)] = 0

  update.end.relative.frequency.2 = post.encounter.2 + relative.frequency.do.not.encounter.2.1 + relative.frequency.do.not.encounter.2.encounter.1

  total.males.surviving.2 = sum(c(update.end.relative.frequency.2))

  update.mean.PRS.2 =  sum((c(update.end.relative.frequency.2) * normal.distribution.values.2))/total.males.surviving.2

  male.selection.differential.2 = update.mean.PRS.2 - trait.mean.2






  return(list(male.selection.differential.1, male.selection.differential.2))

}


