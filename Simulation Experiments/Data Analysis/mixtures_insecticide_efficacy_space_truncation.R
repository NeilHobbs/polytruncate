#0, 5, 10, 20, 50, 80%
library(devtools)
load_all()

library(patchwork)

resistance.i = rep(rep(c(rep(c(0, 47, 100, 225, 900, 3600), each = 169)), 6), 3)
resistance.j = rep(rep(c(0, 47, 100, 225, 900, 3600), each = 1014), 3)
efficacy.i = rep(rep(rep(seq(1.2, 0, -0.1), 13), 36), 3)
efficacy.j = rep(rep(rep(seq(1.2, 0, -0.1), each = 13), 36), 3)
bioassay.i = rep(rep(c(rep(c(0, 5, 10, 20, 50, 80), each = 169)), 6), 3)
bioassay.j = rep(rep(c(0, 5, 10, 20, 50, 80), each = 1014), 3)
female.encounter = rep(c(0.4, 0.7, 0.9), each = 6084)



survival.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                               michaelis.menten.slope = 1,
                                                                                                                               trait.mean = resistance.i,
                                                                                                                               half.population.bioassay.survival.resistance = 900),
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy = efficacy.i)

survival.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                                    michaelis.menten.slope = 1,
                                                                                                                                    trait.mean = resistance.j,
                                                                                                                                    half.population.bioassay.survival.resistance = 900),
                                                                  regression.coefficient = 0.48,
                                                                  regression.intercept = 0.15,
                                                                  current.insecticide.efficacy = efficacy.j)


response.i = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = resistance.i,
                                                                                                         female.fitness.cost = 0,
                                                                                                         male.fitness.cost = 0,
                                                                                                         female.insecticide.exposure = female.encounter,
                                                                                                         male.insecticide.exposure = 0.5,
                                                                                                         z.sd.intercept = 18,
                                                                                                         z.sd.coefficient = 0.4,
                                                                                                         vector.length = 10000,
                                                                                                         maximum.bioassay.survival.proportion = 1,
                                                                                                         michaelis.menten.slope = 1,
                                                                                                         half.population.bioassay.survival.resistance = 900,
                                                                                                         regression.coefficient = 0.48,
                                                                                                         regression.intercept = 0.15,
                                                                                                         current.insecticide.efficacy = efficacy.i,
                                                                                                         exposure.scaling.factor = 1,
                                                                                                         heritability = 0.2,
                                                                                                         survival.to.other.insecticide = survival.j)

response.j = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = resistance.j,
                                                                                                    female.fitness.cost = 0,
                                                                                                    male.fitness.cost = 0,
                                                                                                    female.insecticide.exposure = female.encounter,
                                                                                                    male.insecticide.exposure = 0.5,
                                                                                                    z.sd.intercept = 18,
                                                                                                    z.sd.coefficient = 0.4,
                                                                                                    vector.length = 10000,
                                                                                                    maximum.bioassay.survival.proportion = 1,
                                                                                                    michaelis.menten.slope = 1,
                                                                                                    half.population.bioassay.survival.resistance = 900,
                                                                                                    regression.coefficient = 0.48,
                                                                                                    regression.intercept = 0.15,
                                                                                                    current.insecticide.efficacy = efficacy.j,
                                                                                                    exposure.scaling.factor = 1,
                                                                                                    heritability = 0.2,
                                                                                                    survival.to.other.insecticide = survival.i)

bioassay.change.i = convert_resistance_score_to_bioassay_survival(trait.mean = response.i) -  convert_resistance_score_to_bioassay_survival(trait.mean = resistance.i)
bioassay.change.j = convert_resistance_score_to_bioassay_survival(trait.mean = response.j) -  convert_resistance_score_to_bioassay_survival(trait.mean = resistance.j)


response.df = data.frame(resistance.i, resistance.j, efficacy.i,
                           efficacy.j, response.i, response.j,
                         bioassay.change.i, bioassay.change.j,
                         bioassay.i, bioassay.j, female.encounter)



plot_mixure_efficacy_space = function(X,
                                      encounter.description){

plot.i= ggplot(subset(response.df, female.encounter == X), aes(x=efficacy.i,
                        y=efficacy.j,
                        fill = (bioassay.change.i)*100))+
  geom_raster(interpolate = TRUE)+
  scale_fill_gradient2(high = "darkred", mid="white", low = "skyblue",
                       midpoint = median(subset(response.df, female.encounter == X)$bioassay.change.i*100),
                       name = "Bioassay Change")+
  facet_grid(bioassay.i ~ bioassay.j)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide i",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide j",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Efficacy Insecticide i")+
  ylab("Efficacy Insecticide j")+
  ggtitle("Change for Insecticide i")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.width = unit(2, 'cm'))


plot.j = ggplot(subset(response.df, female.encounter == X), aes(x=efficacy.i,
                                                                y=efficacy.j,
                                                                fill = (bioassay.change.j)*100))+
  geom_raster(interpolate = TRUE)+
  scale_fill_gradient2(high = "darkred", mid="white", low = "skyblue",
                       midpoint = median(subset(response.df, female.encounter == X)$bioassay.change.j*100),
                       name = "Bioassay Change")+
  facet_grid(bioassay.i ~ bioassay.j)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide i",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide j",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Efficacy Insecticide i")+
  ylab("Efficacy Insecticide j")+
  ggtitle("Change for Insecticide j")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.width = unit(2, 'cm'))


plot.ij = ggplot(subset(response.df, female.encounter == X), aes(x=efficacy.i,
                        y=efficacy.j,
                        fill = (bioassay.change.i + bioassay.change.j)*100))+
  geom_raster(interpolate = TRUE)+
  scale_fill_gradient2(high = "darkred", mid="white", low = "skyblue",


                       midpoint = median(
                         subset(response.df, female.encounter == X)$bioassay.change.i +
                         subset(response.df, female.encounter == X)$bioassay.change.j*100),
                       name = "Bioassay Change")+
  facet_grid(bioassay.i ~ bioassay.j)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide i",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%) Insecticide j",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Efficacy Insecticide i")+
  ylab("Efficacy Insecticide j")+
  ggtitle("Total Change: Insecticides i & j")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.width = unit(2, 'cm'))

final.plot = plot.i + plot.j + plot.ij + plot_annotation(title = paste0("polytruncate:", encounter.description))

return(final.plot)
}


low.exp = plot_mixure_efficacy_space(X = 0.4, encounter.description = "Low Encounter")
med.exp = plot_mixure_efficacy_space(X = 0.7, encounter.description = "Moderate Encounter")
high.exp = plot_mixure_efficacy_space(X = 0.9, encounter.description = "High Encounter")


low.exp
med.exp
high.exp


