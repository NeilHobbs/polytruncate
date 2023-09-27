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
                                                                       heritability = 0.3,
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
                                                                                                heritability = 0.3,
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

response.plot.novel = ggplot(response.df, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                        fill = bioassay.change.novel*100))+
  geom_tile()+
  scale_fill_viridis_c(option = "C")+
  facet_grid(start.bioassay.pyrethroid*100)+
  xlab("Efficacy of Novel Insecticide")+
  ylab("Efficacy of Pyrethroid Insecticide")+
  ggtitle("Rate of Evolution Novel Insecticide")+
  labs(fill="Change Bioassay Survival (%)") +
  theme_classic()+
  theme(legend.position = "bottom")


response.plot.pyrethroid = ggplot(response.df, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                        fill = change.pyr.bioassay*100))+
  geom_raster(interpolate = TRUE)+
  scale_fill_viridis_c(option = "C")+
  facet_grid(start.bioassay.pyrethroid*100)+
  xlab("Efficacy of Novel Insecticide")+
  ylab("Efficacy of Pyrethroid Insecticide")+
  labs(fill="Change Bioassay Survival (%)") +
  ggtitle("Rate of Evolution Pyrethroid Insecticide")+
  theme_classic()+
  theme(legend.position = "bottom")

response.plot.pyrethroid

response.plot.combined = ggplot(response.df, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                                                    fill = (bioassay.change.novel*100)+(change.pyr.bioassay*100)))+
  geom_tile()+
  scale_fill_viridis_c(option = "C")+
  xlab("Efficacy of Novel Insecticide")+
  ylab("Efficacy of Pyrethroid Insecticide")+
  facet_grid(start.bioassay.pyrethroid*100)+
  labs(fill="Change Bioassay Survival (%)") +
  ggtitle("Rate of Evolution Both Insecticide")+
  theme_classic()+
  theme(legend.position = "bottom")

response.plot.combined

library(patchwork)

plot.list.pyr = list()
plot.list.novel = list()
plot.list.combined = list()

values = c(0, 0.1, 0.5, 0.8)
for(p in 1:4){

  response.df.1 = response.df%>%
    dplyr::filter(start.bioassay.pyrethroid == values[p])

  plot.list.pyr[[p]] = ggplot(response.df.1, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                                                     fill = change.pyr.bioassay*100))+
    geom_raster(interpolate = TRUE)+
    scale_fill_distiller(palette= "RdYlBu", direction=-1)+
    xlab("Dose Novel Insecticide")+
    ylab("Dose of Pyrethroid")+
    labs(fill="Change Survival (%)") +
    ggtitle("Rate of Evolution Pyrethroid Insecticide")+
    xlim(0, 1)+
    ylim(0, 1)+
    theme_classic()+
    theme(legend.position = "bottom")

  plot.list.novel[[p]] = ggplot(response.df.1, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                                             fill = bioassay.change.novel*100))+
    geom_raster(interpolate = TRUE)+
    scale_fill_distiller(palette= "RdYlBu", direction=-1)+
    xlab("Dose Novel Insecticide")+
    ylab("Dose of Pyrethroid")+
    labs(fill="Change Survival (%)") +
    ggtitle("Rate of Evolution Novel Insecticide")+
    xlim(0, 1)+
    ylim(0, 1)+
    theme_classic()+
    theme(legend.position = "bottom")

  plot.list.combined[[p]] = ggplot(response.df.1, aes(x=efficacy.novel, y=efficacy.pyrethroid,
                                                   fill = (bioassay.change.novel*100)+(change.pyr.bioassay*100)))+
    geom_raster(interpolate = TRUE)+
    scale_fill_distiller(palette= "RdYlBu", direction=-1)+
    xlab("Dose Novel Insecticide")+
    ylab("Dose of Pyrethroid")+
    labs(fill="Change Survival (%)") +
    ggtitle("Combined Rate of Evolution")+
    xlim(0, 1)+
    ylim(0, 1)+
    theme_classic()+
    theme(legend.position = "bottom")

}


plot.layout = "
ABCD
"


plot.list.novel[[1]] + plot.list.novel[[2]] + plot.list.novel[[3]] + plot.list.novel[[4]] +
  plot_layout(design = plot.layout)

plot.list.pyr[[1]] + plot.list.pyr[[2]] + plot.list.pyr[[3]] + plot.list.pyr[[4]] +
  plot_layout(design = plot.layout)

plot.list.combined[[1]] + plot.list.combined[[2]] + plot.list.combined[[3]] + plot.list.combined[[4]] +
  plot_layout(design = plot.layout)

response.df.1 = response.df%>%
  dplyr::filter(efficacy.pyrethroid == 1)

ggplot(response.df, aes(x=efficacy.novel, y=bioassay.change.novel*100,
                        colour = as.numeric(start.bioassay.pyrethroid)))+
  geom_point()+
  facet_wrap(~efficacy.pyrethroid)+
  theme_bw()+
  theme(legend.position = "none")







