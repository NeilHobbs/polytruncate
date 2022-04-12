#Note this is not giving the same response as with the formal truncation selection methodology,
#it is out in the calculation of the new PRS mean by often only a small amount but this equates to
#large-ish differences over the course of multiple generations. [Note that providing n.cycles=1 as the
#comparitor this should not actually matter. E.g. this is the default methodology]

#We would expect n.cycles =  1 to perform identically to the standard truncation selection methodology.

perform_micro_mosaics_truncation = function(trait.mean.1,
                                            trait.mean.2,
                                            standard.deviation,
                                            vector.length,
                                            n.cycles,
                                            female.exposure,
                                            coverage.1,
                                            coverage.2,
                                            male.selection.differential.1,
                                            male.selection.differential.2,
                                            heritability,
                                            exposure.scaling.factor,
                                            maximum.bioassay.survival.proportion,
                                            half.population.bioassay.survival.resistance,
                                            michaelis.menten.slope,
                                            regression.coefficient,
                                            regression.intercept,
                                            current.insecticide.efficacy.1,
                                            current.insecticide.efficacy.2,
                                            cross.selection){

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


  do.not.encounter = 1 - ((coverage.1 + coverage.2) * female.exposure)

  relative.frequency.encounter.1 = list()
  relative.frequency.do.not.encounter.1.2 = list()
  relative.frequency.do.not.encounter.1.encounter.2 = list()
  update.end.relative.frequency.1 = list()
  total.females.surviving.1 = list()
  update.mean.PRS.1 = list()
  female.selection.differential.1 = list()
  response.value.1 = list()
  vector.position.1 = c()

  relative.frequency.encounter.2 = list()
  relative.frequency.do.not.encounter.2.1 = list()
  relative.frequency.do.not.encounter.2.encounter.1 = list()
  update.end.relative.frequency.2 = list()
  total.females.surviving.2 = list()
  female.selection.differential.2 = list()
  update.mean.PRS.2 = list()
  response.value.2 = list()
  vector.position.2 = c()


  for(gonotrophic in 1:n.cycles){

    if(gonotrophic == 1){

      #Values needed for Trait 1 [assumes traits are unlinked]
      relative.frequency.do.not.encounter.1.2[[gonotrophic]] = do.not.encounter*relative.trait.frequency.1
      relative.frequency.encounter.1[[gonotrophic]] = female.exposure * coverage.1 * relative.trait.frequency.1
      relative.frequency.do.not.encounter.1.encounter.2[[gonotrophic]] = female.exposure * coverage.2 * relative.trait.frequency.1 * survival.rate.2

      total.females.surviving.exposure.1 = (sum(relative.frequency.encounter.1[[gonotrophic]])*survival.rate.1)

      cumulative.frequency.1 = cumsum(rev(c(relative.frequency.encounter.1[[gonotrophic]])))

      vector.position.1[gonotrophic] = which(abs(cumulative.frequency.1 - total.females.surviving.exposure.1) == min(abs(cumulative.frequency.1 - total.females.surviving.exposure.1)))

      #those below the threshold all die.
      post.encounter.1 = relative.frequency.encounter.1[[gonotrophic]]
      post.encounter.1[0:(vector.length-vector.position.1[gonotrophic])] = 0

      update.end.relative.frequency.1[[gonotrophic]] = post.encounter.1 + relative.frequency.do.not.encounter.1.2[[gonotrophic]] + relative.frequency.do.not.encounter.1.encounter.2[[gonotrophic]]

      total.females.surviving.1[[gonotrophic]] = sum(c(update.end.relative.frequency.1[[gonotrophic]]))

      update.mean.PRS.1[[gonotrophic]] =  sum((c(update.end.relative.frequency.1[[gonotrophic]]) * normal.distribution.values.1))/total.females.surviving.1[[gonotrophic]]

      female.selection.differential.1[[gonotrophic]] = update.mean.PRS.1[[gonotrophic]] - trait.mean.1

      response.value.1[[gonotrophic]] = exposure.scaling.factor*heritability * ((female.selection.differential.1[[gonotrophic]] + male.selection.differential.1) / 2)


      #Values needed for Trait 2
      relative.frequency.do.not.encounter.2.1[[gonotrophic]] = do.not.encounter*relative.trait.frequency.2
      relative.frequency.encounter.2[[gonotrophic]] = female.exposure * coverage.2 * relative.trait.frequency.2
      relative.frequency.do.not.encounter.2.encounter.1[[gonotrophic]] = female.exposure * coverage.1 * relative.trait.frequency.2 * survival.rate.1

      total.females.surviving.exposure.2 = (sum(relative.frequency.encounter.2[[gonotrophic]])*survival.rate.2)

      cumulative.frequency.2 = cumsum(rev(c(relative.frequency.encounter.2[[gonotrophic]])))

      vector.position.2[gonotrophic] = which(abs(cumulative.frequency.2 - total.females.surviving.exposure.2) == min(abs(cumulative.frequency.2 - total.females.surviving.exposure.2)))

      #those below the threshold all die.
      post.encounter.2 = relative.frequency.encounter.2[[gonotrophic]]
      post.encounter.2[0:(vector.length-vector.position.2[gonotrophic])] = 0

      update.end.relative.frequency.2[[gonotrophic]] = post.encounter.2 + relative.frequency.do.not.encounter.2.1[[gonotrophic]] + relative.frequency.do.not.encounter.2.encounter.1[[gonotrophic]]

      total.females.surviving.2[[gonotrophic]] = sum(c(update.end.relative.frequency.2[[gonotrophic]]))

      update.mean.PRS.2[[gonotrophic]] =  sum((c(update.end.relative.frequency.2[[gonotrophic]]) * normal.distribution.values.2))/total.females.surviving.2[[gonotrophic]]

      female.selection.differential.2[[gonotrophic]] = update.mean.PRS.2[[gonotrophic]] - trait.mean.2

      response.value.2[[gonotrophic]] = exposure.scaling.factor*heritability * ((female.selection.differential.2[[gonotrophic]] + male.selection.differential.2) / 2)

      }

    if(gonotrophic != 1){
      #Values needed for Trait 1 [assumes traits are unlinked]
      relative.frequency.do.not.encounter.1.2[[gonotrophic]] = do.not.encounter*update.end.relative.frequency.1[[gonotrophic-1]]
      relative.frequency.encounter.1[[gonotrophic]] = female.exposure * coverage.1 * update.end.relative.frequency.1[[gonotrophic-1]]
      relative.frequency.do.not.encounter.1.encounter.2[[gonotrophic]] = female.exposure * coverage.2 * update.end.relative.frequency.1[[gonotrophic-1]] * survival.rate.2

      total.females.surviving.exposure.1 = (sum(relative.frequency.encounter.1[[gonotrophic]])*survival.rate.1)

      cumulative.frequency.1 = cumsum(rev(c(relative.frequency.encounter.1[[gonotrophic]])))

      vector.position.1[gonotrophic] = which(abs(cumulative.frequency.1 - total.females.surviving.exposure.1) == min(abs(cumulative.frequency.1 - total.females.surviving.exposure.1)))

      #those below the threshold all die.
      post.encounter.1 = relative.frequency.encounter.1[[gonotrophic]]
      post.encounter.1[0:(vector.length-vector.position.1[gonotrophic])] = 0

      update.end.relative.frequency.1[[gonotrophic]] = post.encounter.1 + c(relative.frequency.do.not.encounter.1.2[[gonotrophic]]) + c(relative.frequency.do.not.encounter.1.encounter.2[[gonotrophic]])

      total.females.surviving.1[[gonotrophic]] = sum(c(update.end.relative.frequency.1[[gonotrophic]]))

      update.mean.PRS.1[[gonotrophic]] =  sum((c(update.end.relative.frequency.1[[gonotrophic]]) * normal.distribution.values.1))/total.females.surviving.1[[gonotrophic]]

      female.selection.differential.1[[gonotrophic]] = update.mean.PRS.1[[gonotrophic]] - trait.mean.1

      response.value.1[[gonotrophic]] = exposure.scaling.factor*heritability * ((female.selection.differential.1[[gonotrophic]] + male.selection.differential.1) / 2)


      #Values needed for Trait 2
      relative.frequency.do.not.encounter.2.1[[gonotrophic]] = do.not.encounter*update.end.relative.frequency.2[[gonotrophic-1]]
      relative.frequency.encounter.2[[gonotrophic]] = female.exposure * coverage.2 * update.end.relative.frequency.2[[gonotrophic-1]]
      relative.frequency.do.not.encounter.2.encounter.1[[gonotrophic]] = female.exposure * coverage.1 * update.end.relative.frequency.2[[gonotrophic-1]] * survival.rate.2

      total.females.surviving.exposure.2 = (sum(relative.frequency.encounter.2[[gonotrophic]])*survival.rate.2)

      cumulative.frequency.2 = cumsum(rev(c(relative.frequency.encounter.2[[gonotrophic]])))

      vector.position.2[gonotrophic] = which(abs(cumulative.frequency.2 - total.females.surviving.exposure.2) == min(abs(cumulative.frequency.2 - total.females.surviving.exposure.2)))

      #those below the threshold all die.
      post.encounter.2 = relative.frequency.encounter.2[[gonotrophic]]
      post.encounter.2[0:(vector.length-vector.position.2[gonotrophic])] = 0

      update.end.relative.frequency.2[[gonotrophic]] = post.encounter.2 + c(relative.frequency.do.not.encounter.2.1[[gonotrophic]]) + c(relative.frequency.do.not.encounter.2.encounter.1[[gonotrophic]])

      total.females.surviving.2[[gonotrophic]] = sum(c(update.end.relative.frequency.2[[gonotrophic]]))

      update.mean.PRS.2[[gonotrophic]] =  sum((c(update.end.relative.frequency.2[[gonotrophic]]) * normal.distribution.values.2))/total.females.surviving.2[[gonotrophic]]

      female.selection.differential.2[[gonotrophic]] = update.mean.PRS.2[[gonotrophic]] - trait.mean.2

      response.value.2[[gonotrophic]] = exposure.scaling.factor*heritability * ((female.selection.differential.2[[gonotrophic]] + male.selection.differential.2) / 2)
    }

  }

  new.mean.1 = unlist(update.mean.PRS.1)
  response.values.1 = unlist(response.value.1)
  pop.size.values.1 = unlist(total.females.surviving.1)

  new.mean.2 = unlist(update.mean.PRS.2)
  response.values.2 = unlist(response.value.2)
  pop.size.values.2 = unlist(total.females.surviving.2)


  total.oviposition.1 = sum(pop.size.values.1)
  overall.response.1 = sum(response.values.1*(pop.size.values.1/total.oviposition.1))

  total.oviposition.2 = sum(pop.size.values.2)
  overall.response.2 = sum(response.values.2*(pop.size.values.2/total.oviposition.2))

  #allow for cross selection:
  overall.response.1.cs = overall.response.1 + (cross.selection * overall.response.2)
  overall.response.2.cs = overall.response.2 + (cross.selection * overall.response.1)


  df.1 = data.frame(overall.response.1.cs, overall.response.2.cs)
  df.2 = data.frame(new.mean.1, response.values.1, pop.size.values.1,
                    new.mean.2, response.values.2, pop.size.values.2)

  return(df.1)

}
#
# insecticide.coverage.1 = seq(0, 1, by = 0.1)
# insecticide.coverage.2 = seq(1, 0, by = -0.1)
#
#
# temp.list = list()
# for(i in 1:length(insecticide.coverage.1)){
#
#   temp.list[[i]] = perform_micro_mosaics_truncation(trait.mean.1 = 0,
#                                                     trait.mean.2 = 0,
#                                                     standard.deviation = 20,
#                                                     vector.length = 10000,
#                                                     n.cycles = 10,
#                                                     female.exposure = 1,
#                                                     coverage.1 = insecticide.coverage.1[i],
#                                                     coverage.2 = insecticide.coverage.2[i],
#                                                     male.selection.differential.1 = 0,
#                                                     male.selection.differential.2 = 0,
#                                                     heritability =0.3,
#                                                     exposure.scaling.factor = 1,
#                                                     maximum.bioassay.survival.proportion =1,
#                                                     half.population.bioassay.survival.resistance = 900,
#                                                     michaelis.menten.slope = 1,
#                                                     regression.coefficient = 0.48,
#                                                     regression.intercept = 0.15,
#                                                     current.insecticide.efficacy.1 = 1,
#                                                     current.insecticide.efficacy.2 = 1)
# }
#
#
#
#
# df.temp = data.frame(do.call(rbind, temp.list), insecticide.coverage.1, insecticide.coverage.2)
#
# df.temp$total.response = df.temp$overall.response.1 + df.temp$overall.response.2
# df.temp$mean.response = (df.temp$overall.response.1 + df.temp$overall.response.2)/2
#
#
#
# ggplot(df.temp, aes(x=insecticide.coverage.1, y=overall.response.1))+
#   geom_line(colour = "red")+
#   geom_line(aes(x=insecticide.coverage.1, y=overall.response.2),
#             colour = "blue")+
#   geom_line(aes(x=insecticide.coverage.1, y = total.response),
#             colour = "purple")+
#   geom_line(aes(x=insecticide.coverage.1, y = mean.response),
#             colour = "yellow")+
#   xlab("Insecticide Coverage 1")+
#   ylab("Response")+
#   theme_classic()

