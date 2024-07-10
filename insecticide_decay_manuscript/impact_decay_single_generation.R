library(devtools)
load_all()

initial.resistance.novel = rep(0, (121))
initial.resistance.pyrethroid = c(rep(0, 121), rep(47, 121), rep(100, 121), rep(225, 121), rep(900, 121), rep(3600, 121))

efficacy.novel = rep(rep(seq(1, 0, by = -0.1), 11), 6)

#Do for all parameter value sets
#Calculate Mean + 95%CI  and plot





eff.list = list()
for(i in 1:11){
  eff.list[[i]] = rep(efficacy.novel[i], 11)
}

efficacy.pyrethroid = rep(unlist(eff.list), 6)

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


parameter_space_smooth = read.csv(("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv"))

response.list = list()
for(j in 1:5000){
pyrethroid.response = c()
novel.response = c()
  pyrethroid.response = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.pyrethroid[i],
                                                                                                            female.fitness.cost = 0,
                                                                                                            male.fitness.cost = 0,
                                                                                                            female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j],
                                                                                                            male.insecticide.exposure =parameter_space_smooth$Male.Insecticide.Exposure[j],
                                                                                                            z.sd.intercept = 18,
                                                                                                            z.sd.coefficient = 0.4,
                                                                                                            vector.length = 250,
                                                                                                            maximum.bioassay.survival.proportion = 1,
                                                                                                            michaelis.menten.slope = 1,
                                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                                            regression.coefficient = 0.48,
                                                                                                            regression.intercept = 0.15,
                                                                                                            current.insecticide.efficacy = efficacy.pyrethroid,
                                                                                                            exposure.scaling.factor = 1,
                                                                                                            heritability = parameter_space_smooth$Heritability[j],
                                                                                                            survival.to.other.insecticide = novel.survival)

  novel.response = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.novel,
                                                                                                       female.fitness.cost = 0,
                                                                                                       male.fitness.cost = 0,
                                                                                                       female.insecticide.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j],
                                                                                                       male.insecticide.exposure =parameter_space_smooth$Male.Insecticide.Exposure[j],
                                                                                                       z.sd.intercept = 18,
                                                                                                       z.sd.coefficient = 0.4,
                                                                                                       vector.length = 250,
                                                                                                       maximum.bioassay.survival.proportion = 1,
                                                                                                       michaelis.menten.slope = 1,
                                                                                                       half.population.bioassay.survival.resistance = 900,
                                                                                                       regression.coefficient = 0.48,
                                                                                                       regression.intercept = 0.15,
                                                                                                       current.insecticide.efficacy = efficacy.novel,
                                                                                                       exposure.scaling.factor = 1,
                                                                                                       heritability = parameter_space_smooth$Heritability[j],
                                                                                                       survival.to.other.insecticide = pyrethroid.survival)


  print(j)

  response.df = data.frame(novel.response, pyrethroid.response, efficacy.novel, efficacy.pyrethroid, initial.resistance.novel, initial.resistance.pyrethroid,
                           heritability = rep(parameter_space_smooth$Heritability[j], 726),
                           female.insecticide.exposure = rep(parameter_space_smooth$Female.Insecticide.Exposure[j], 726),
                           male.insecticide.exposure =rep(parameter_space_smooth$Male.Insecticide.Exposure[j], 726))

  response.list[[j]] = response.df

  print(j)
}


response.df = do.call(rbind, response.list)

rm(response.list)

response.df$bioassay.change.novel = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                      michaelis.menten.slope = 1,
                                                                      trait.mean = response.df$novel.response,
                                                                      half.population.bioassay.survival.resistance = 900)

response.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                          michaelis.menten.slope = 1,
                                                                          trait.mean = response.df$initial.resistance.pyrethroid,
                                                                          half.population.bioassay.survival.resistance = 900)

response.df$end.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1,
                                                                        trait.mean = response.df$pyrethroid.response,
                                                                        half.population.bioassay.survival.resistance = 900)


response.df$change.pyr.bioassay = c(response.df$end.bioassay.pyrethroid - response.df$start.bioassay.pyrethroid)

response.df.novel= response.df%>%
  group_by(start.bioassay.pyrethroid,
           efficacy.novel, efficacy.pyrethroid)%>%
  summarise(mean.value = mean(bioassay.change.novel*100),
    upper.ci = quantile(bioassay.change.novel*100, 0.95),
    lower.ci = quantile(bioassay.change.novel*100, 0.05),
    standard.dev = sd(bioassay.change.novel*100))

response.df.pyrethroid= response.df%>%
  group_by(start.bioassay.pyrethroid,
           efficacy.novel, efficacy.pyrethroid)%>%
  summarise(mean.value = mean(change.pyr.bioassay*100),
            upper.ci = quantile(change.pyr.bioassay*100, 0.95),
            lower.ci = quantile(change.pyr.bioassay*100, 0.05),
            standard.dev = sd(change.pyr.bioassay*100))

response.df.overall = response.df%>%
  group_by(start.bioassay.pyrethroid,
           efficacy.novel, efficacy.pyrethroid)%>%
  summarise(mean.value = mean((bioassay.change.novel+change.pyr.bioassay)*100),
            upper.ci = quantile((bioassay.change.novel+change.pyr.bioassay)*100, 0.95),
            lower.ci = quantile((bioassay.change.novel+change.pyr.bioassay)*100, 0.05),
            standard.dev = sd((bioassay.change.novel+change.pyr.bioassay)*100))


response.df.novel$mean.value = ifelse(response.df.novel$mean.value == 0,
                                      yes = NA,
                                      no = response.df.novel$mean.value)


range(response.df.novel$standard.dev)


param.1 = response.df[1:1764, ]
param.2 = response.df[1765:(1764+1764), ]

ggplot(param.1, aes(x= efficacy.novel,
                              y= efficacy.pyrethroid,
                              fill = novel.response))+
  geom_tile()+
  scale_fill_viridis_c(na.value = "grey",
                       direction = 1,
                       option = "inferno",
                       limits = c(0, 4))+
  facet_wrap(. ~ (start.bioassay.pyrethroid*100))+
  theme_classic()

ggplot(param.2, aes(x= efficacy.novel,
                    y= efficacy.pyrethroid,
                    fill = novel.response))+
  geom_tile()+
  scale_fill_viridis_c(na.value = "grey",
                       direction = 1,
                       option = "inferno",
                       limits = c(0, 2))+
  facet_wrap(. ~ (start.bioassay.pyrethroid*100))+
  theme_classic()



temp.df = subset(response.df, efficacy.novel > 0)

response.df$efficacy.novel = round(response.df$efficacy.novel, 2)
response.df$efficacy.pyrethroid = round(response.df$efficacy.pyrethroid, 2)

table(response.df$efficacy.novel)

small.df = sample_n(temp.df, 50000)

ggplot(response.df, aes(x=female.insecticide.exposure,
                        y=bioassay.change.novel*100,
                        colour = as.factor(efficacy.novel),
                    fill = as.factor(efficacy.novel)))+
  geom_smooth(method = "gam",  method.args=list(family="nb"))+
  xlab("Female Insecticide Exposure")+
  ylab("Response as Bioassay Survival (%)")+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  facet_wrap((start.bioassay.pyrethroid*100~.))+
  theme_classic()+
  theme(legend.position = "none")


ggplot(response.df, aes(x=female.insecticide.exposure,
                     y=bioassay.change.novel*100,
                     colour = as.factor(start.bioassay.pyrethroid*100),
                     fill = as.factor(start.bioassay.pyrethroid*100)))+
  geom_smooth(method = "gam",  method.args=list(family="nb"))+
  xlab("Female Insecticide Exposure")+
  ylab("Response as Bioassay Survival (%)")+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  facet_wrap((efficacy.novel~.))+
  theme_classic()+
  theme(legend.position = "none")





ggplot(response.df, aes(x=female.insecticide.exposure,
                    y=bioassay.change.novel*100,
                    colour = as.factor(efficacy.pyrethroid),
                    fill = as.factor(efficacy.pyrethroid)))+
  geom_smooth(method = "gam",  method.args=list(family="nb"))+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  xlab("Female Insecticide Exposure")+
  ylab("Response as Bioassay Survival (%)")+
  facet_wrap((start.bioassay.pyrethroid*100)~.)+
  theme_classic()+
  theme(legend.position = "none")


#The purpose of this is analysis is to describe and explains
#how the rate of evolution can be changed with regards to the dose
#of insecticide encountered.

ggplot(small.df, aes(x=as.character(efficacy.novel),
                        y=bioassay.change.novel))+
  geom_boxplot()+
  facet_wrap(efficacy.pyrethroid ~ .)







param.1 = response.df[1:1764, ]
param.2 = response.df[1765:(1764+1764), ]

param.1 = subset(response.df, heritability > 0.2)


ggplot(param.2, aes(x= efficacy.novel,
                    y= efficacy.pyrethroid,
                    fill = (bioassay.change.novel*100)))+
  geom_tile()+
  scale_fill_viridis_c(na.value = "grey",
                       direction = 1,
                       option = "inferno")+
  facet_wrap(. ~ (start.bioassay.pyrethroid*100))+
  theme_classic()



small.df = subset(response.df, efficacy.pyrethroid < 0.2)

table(small.df$start.bioassay.pyrethroid)


ggplot(response.df, aes(x=efficacy.novel,
                        y=bioassay.change.novel*100,
                        colour = as.factor(efficacy.pyrethroid),
                        fill = as.factor(efficacy.pyrethroid)))+
  geom_smooth(method="gam")+
  scale_colour_viridis_d(option = "plasma", direction = -1)+
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  facet_grid(.~(start.bioassay.pyrethroid*100))+
  ylab("Response as Change in Bioassay Survival (%)")+
  xlab("Dose of Novel Insecticide")+
  guides(fill=guide_legend(title="Pyrethoid Dose"),
         colour = guide_legend(title="Pyrethoid Dose"))+
  theme_bw()+
  theme(legend.position = "bottom")


ggplot(response.df, aes(x=efficacy.pyrethroid,
                        y=change.pyr.bioassay*100,
                        colour = as.factor(efficacy.novel),
                        fill = as.factor(efficacy.novel)))+
  geom_smooth(method="gam")+
  scale_colour_viridis_d(option = "plasma", direction = -1)+
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  facet_grid(.~(start.bioassay.pyrethroid*100))+
  ylab("Response as Change in Bioassay Survival (%)")+
  xlab("Dose of Novel Insecticide")+
  guides(fill=guide_legend(title="Novel Dose"),
         colour = guide_legend(title="Novel Dose"))+
  theme_bw()+
  theme(legend.position = "bottom")



ggplot(response.df, aes(x=heritability,
                        y=end.bioassay.pyrethroid*100,
                        colour = as.factor(efficacy.novel),
                        fill = as.factor(efficacy.novel)))+
  geom_smooth(method="gam")+
  scale_colour_viridis_d(option = "plasma", direction = -1)+
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  facet_grid(efficacy.pyrethroid~(start.bioassay.pyrethroid*100))+
  ylab("Response as Change in Bioassay Survival (%)")+
  xlab("Dose of Novel Insecticide")+
  guides(fill=guide_legend(title="Novel Dose"),
         colour = guide_legend(title="Novel Dose"))+
  theme_bw()+
  theme(legend.position = "bottom")



small.df = sample_n(response.df, 5000)


ggplot(response.df, aes(x=efficacy.pyrethroid,
                        y=end.bioassay.pyrethroid*100,
                        colour = as.factor(efficacy.novel),
                        fill = as.factor(efficacy.novel)))+
  geom_smooth(method = "gam")+
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_colour_viridis_d(option = "plasma", direction = -1)+
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  ylab("Response as Change in Bioassay Survival (%)")+
  guides(fill=guide_legend(title="Novel Dose"),
         colour = guide_legend(title="Novel Dose"))+
  theme_bw()+
  theme(legend.position = "bottom")
















