library(devtools)
load_all()

initial.resistance.novel = rep(0, 2500)
initial.resistance.pyrethroid = c(rep(0, 625), rep(100, 625), rep(900, 625), rep(3600, 625))

efficacy.novel = rep(rep(seq(1.2, 0, by = -0.05), 25), 4)

#Do for all parameter value sets
#Calculate Mean + 95%CI  and plot

eff.list = list()
for(i in 1:25){
  eff.list[[i]] = rep(efficacy.novel[i], 25)
}

efficacy.pyrethroid = rep(unlist(eff.list), 4)

novel.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                               michaelis.menten.slope = 1,
                                                                                                                               trait.mean = initial.resistance.novel,
                                                                                                                               half.population.bioassay.survival.resistance = 900),
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy = efficacy.novel)

pyrethroid.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                                    michaelis.menten.slope = 1,
                                                                                                                                    trait.mean = initial.resistance.pyrethroid,
                                                                                                                                    half.population.bioassay.survival.resistance = 900),
                                                                  regression.coefficient = 0.48,
                                                                  regression.intercept = 0.15,
                                                                  current.insecticide.efficacy = efficacy.pyrethroid)

pyrethroid.response = c()
novel.response = c()

for(i in 1:2500){
  pyrethroid.response[i] = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.pyrethroid[i],
                                                                                                            female.fitness.cost = 0,
                                                                                                            male.fitness.cost = 0,
                                                                                                            female.insecticide.exposure = 0.7,
                                                                                                            male.insecticide.exposure =0.7,
                                                                                                            z.sd.intercept = 18,
                                                                                                            z.sd.coefficient = 0.4,
                                                                                                            vector.length = 1000,
                                                                                                            maximum.bioassay.survival.proportion = 1,
                                                                                                            michaelis.menten.slope = 1,
                                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                                            regression.coefficient = 0.48,
                                                                                                            regression.intercept = 0.15,
                                                                                                            current.insecticide.efficacy = efficacy.pyrethroid[i],
                                                                                                            exposure.scaling.factor = 1,
                                                                                                            heritability = 0.2,
                                                                                                            survival.to.other.insecticide = novel.survival[i])

  novel.response[i] = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.novel[i],
                                                                                                       female.fitness.cost = 0,
                                                                                                       male.fitness.cost = 0,
                                                                                                       female.insecticide.exposure = 0.7,
                                                                                                       male.insecticide.exposure =0.7,
                                                                                                       z.sd.intercept = 18,
                                                                                                       z.sd.coefficient = 0.4,
                                                                                                       vector.length = 1000,
                                                                                                       maximum.bioassay.survival.proportion = 1,
                                                                                                       michaelis.menten.slope = 1,
                                                                                                       half.population.bioassay.survival.resistance = 900,
                                                                                                       regression.coefficient = 0.48,
                                                                                                       regression.intercept = 0.15,
                                                                                                       current.insecticide.efficacy = efficacy.novel[i],
                                                                                                       exposure.scaling.factor = 1,
                                                                                                       heritability = 0.2,
                                                                                                       survival.to.other.insecticide = pyrethroid.survival[i])

  print(i)
}



response.df = data.frame(novel.response, pyrethroid.response, efficacy.novel, efficacy.pyrethroid, initial.resistance.novel, initial.resistance.pyrethroid)

bioassay.change.novel = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                      michaelis.menten.slope = 1,
                                                                      trait.mean = novel.response,
                                                                      half.population.bioassay.survival.resistance = 900)

start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                          michaelis.menten.slope = 1,
                                                                          trait.mean = initial.resistance.pyrethroid,
                                                                          half.population.bioassay.survival.resistance = 900)

end.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1,
                                                                        trait.mean = pyrethroid.response,
                                                                        half.population.bioassay.survival.resistance = 900)


change.pyr.bioassay = c(end.bioassay.pyrethroid - start.bioassay.pyrethroid)


response.df = data.frame(response.df, bioassay.change.novel, start.bioassay.pyrethroid, end.bioassay.pyrethroid, change.pyr.bioassay)


total.plot= ggplot(response.df, aes(x=efficacy.novel,
                        y=(bioassay.change.novel+change.pyr.bioassay)*100,
                        colour = as.character((round(efficacy.pyrethroid, 2))),
                        fill = as.character((round(efficacy.pyrethroid, 2)))))+
  geom_line(size = 1.2)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  ylab("Total Bioassay Change")+
  xlab("Novel Insecticide Efficacy")+
  guides(colour=guide_legend(title="Pyrethroid Efficacy",
                             nrow = 2))+
  facet_grid(. ~(start.bioassay.pyrethroid*100))+
  theme_bw()+
  theme(legend.position = "bottom")


novel.plot = ggplot(response.df, aes(x=efficacy.novel,
                        y=bioassay.change.novel*100,
                        colour = as.character((round(efficacy.pyrethroid, 2))),
                        fill = as.character((round(efficacy.pyrethroid, 2)))))+
  geom_line(size =1.2)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  ylab("Novel Bioassay Change")+
  xlab("Novel Insecticide Efficacy")+
  guides(colour=guide_legend(title="Pyrethroid Efficacy"))+
  facet_grid(. ~(start.bioassay.pyrethroid*100))+
  theme_bw()+
  theme(legend.position = "none")

pyr.plot = ggplot(response.df, aes(x=efficacy.novel,
                        y=change.pyr.bioassay*100,
                        colour = as.character((round(efficacy.pyrethroid, 2))),
                        fill = as.character((round(efficacy.pyrethroid, 2)))))+
  geom_line(size = 1.2)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  ylab("Pyrethroid Bioassay Change")+
  xlab("Novel Insecticide Efficacy")+
  guides(colour=guide_legend(title="Pyrethroid Efficacy",
                             nrow = 2))+
  facet_grid(. ~(start.bioassay.pyrethroid*100))+
  theme_bw()+
  theme(legend.position = "none")


novel.plot / pyr.plot / total.plot +
 plot_annotation(title = "polytruncate")



