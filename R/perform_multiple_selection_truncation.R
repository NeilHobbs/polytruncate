#Note this is not giving the same response as with the formal truncation selection methodology,
  #it is out in the calculation of the new PRS mean by often only a small amount but this equates to
  #large errors over the course of multiple generations.

#We would expect n.cycles =  1 to perform identically to the standard truncation selection methodology.

perform_multiple_selection_truncation = function(trait.mean,
                                                 standard.deviation,
                                                 vector.length,
                                                 n.cycles,
                                                 female.exposure,
                                                 male.selection.differential,
                                                 heritability,
                                                 exposure.scaling.factor,
                                                 maximum.bioassay.survival.proportion,
                                                 half.population.bioassay.survival.resistance,
                                                 michaelis.menten.slope,
                                                 regression.coefficient,
                                                 regression.intercept,
                                                 current.insecticide.efficacy){

  normal.distribution.values = create_normal_distribution(vector.length = vector.length,
                                                          trait.mean = trait.mean,
                                                          standard.deviation = standard.deviation)

  relative.trait.frequency = calculate_density_of_trait_values(vector.length = vector.length,
                                                               trait.mean = trait.mean,
                                                               standard.deviation = standard.deviation)

  initial.population.size = sum(relative.trait.frequency)

  survival.rate = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                trait.mean = trait.mean,
                                                                                                                                michaelis.menten.slope = michaelis.menten.slope),
                                                              regression.coefficient = regression.coefficient,
                                                              regression.intercept = regression.intercept,
                                                              current.insecticide.efficacy = current.insecticide.efficacy)

  relative.frequency.encounter = list()
  relative.frequency.do.not.encounter = list()
  update.end.relative.frequency = list()
  total.females.surviving = list()
  update.mean.PRS = list()
  response.value = list()
  vector.position = c()


  for(gonotrophic in 1:n.cycles){
    #the total that encounter the insecticide
    if(gonotrophic == 1){
      relative.frequency.do.not.encounter[[gonotrophic]] = (1 - female.exposure)*relative.trait.frequency
      relative.frequency.encounter[[gonotrophic]] = female.exposure * relative.trait.frequency
    }
    if(gonotrophic != 1){
    relative.frequency.do.not.encounter[[gonotrophic]] = (1 - female.exposure)*update.end.relative.frequency[[gonotrophic-1]]
    relative.frequency.encounter[[gonotrophic]] = female.exposure * update.end.relative.frequency[[gonotrophic-1]]
    }

    gonotrophic.survival.rate = ifelse(gonotrophic == 1,
                           yes = survival.rate,
                           no =convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             trait.mean = update.mean.PRS[[gonotrophic-1]],
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy))

    #total.surviving = sum(c(relative.frequency.encounter[[gonotrophic]])) * gonotrophic.survival.rate

    total.surviving = sum(relative.trait.frequency) * ((female.exposure*survival.rate*survival.rate)^gonotrophic)

    cumulative.frequency = cumsum(rev(c(relative.frequency.encounter[[gonotrophic]])))

    vector.position[gonotrophic] = which(abs(cumulative.frequency - total.surviving) == min(abs(cumulative.frequency - total.surviving)))

    relative.frequency.surviving.encounter = relative.frequency.encounter
    #those below the threshold all die.
    post.encounter[0:(vector.length-vector.position[gonotrophic])] = 0

    update.end.relative.frequency[[gonotrophic]] = post.encounter + c(relative.frequency.do.not.encounter[[gonotrophic]])

    total.females.surviving[[gonotrophic]] = sum(c(update.end.relative.frequency[[gonotrophic]]))

    update.mean.PRS[[gonotrophic]] =  sum((c(update.end.relative.frequency[[gonotrophic]]) * normal.distribution.values))/total.females.surviving[[gonotrophic]]

    female.selection.differential = update.mean.PRS[[gonotrophic]] - trait.mean

    response.value[[gonotrophic]] = exposure.scaling.factor*heritability * ((female.selection.differential + male.selection.differential) / 2)

  }

  new.mean = unlist(update.mean.PRS)
  response.values = unlist(response.value)
  pop.size.values = unlist(total.females.surviving)

  return(list(response.values, pop.size.values, update.end.relative.frequency, normal.distribution.values))

}

A = perform_multiple_selection_truncation(trait.mean = 0,
                                      standard.deviation = 30,
                                      vector.length = 10000,
                                      n.cycles = 5,
                                      female.exposure = 0.5,
                                      male.selection.differential = 0,
                                      heritability = 0.3,
                                      exposure.scaling.factor = 1,
                                      maximum.bioassay.survival.proportion = 1,
                                      half.population.bioassay.survival.resistance = 900,
                                      michaelis.menten.slope = 1,
                                      regression.coefficient = 0.48,
                                      regression.intercept = 0.15,
                                      current.insecticide.efficacy = 1)

library(ggplot2)
gonotrophic.1 = A[[3]][[1]]
gonotrophic.2 = A[[3]][[2]]
gonotrophic.3 = A[[3]][[3]]
gonotrophic.4 = A[[3]][[4]]
gonotrophic.5 = A[[3]][[5]]
norm.dist = A[[4]]

df = data.frame(gonotrophic.1, gonotrophic.2, gonotrophic.3,
                gonotrophic.4, gonotrophic.5, norm.dist)


ggplot(df, aes(x=norm.dist, y=gonotrophic.1))+
  geom_line(colour = "red", size = 2,
            alpha = 0.7)+
  geom_line(aes(x=norm.dist, y=gonotrophic.2),
            colour = "green", size = 2,
            alpha = 0.7)+
  geom_line(aes(x=norm.dist, y=gonotrophic.3),
            colour = "blue", size = 2,
            alpha = 0.7)+
  geom_line(aes(x=norm.dist, y=gonotrophic.4),
            colour = "orange", size = 2,
            alpha = 0.7)+
  geom_line(aes(x=norm.dist, y=gonotrophic.5),
            colour = "yellow", size = 2,
            alpha = 0.7)+
  xlab("PRS Value")+
  ylab("Relative Frequency")+
  theme_classic()

