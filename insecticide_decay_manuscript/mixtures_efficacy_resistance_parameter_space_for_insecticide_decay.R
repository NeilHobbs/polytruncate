library(devtools)
load_all()
library(patchwork)
#starting bioassay survival was set as 0, 5, 10, 20, 50 or 80%

initial.resistance.i = rep(c(0, 47, 100, 225, 900, 3600), each = 1014)
initial.resistance.j = rep(rep(c(0, 47, 100, 225, 900, 3600), each = 169), 6)

efficacy.i = rep(rep(rep(seq(0, 1.2, 0.1), 13), 6), 6)
efficacy.j = rep(rep(rep(seq(0, 1.2, 0.1), each = 13), 6), 6)


i.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                           michaelis.menten.slope = 1,
                                                                                                                           trait.mean = initial.resistance.i,
                                                                                                                           half.population.bioassay.survival.resistance = 900),
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = efficacy.i)

j.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                           michaelis.menten.slope = 1,
                                                                                                                           trait.mean = initial.resistance.j,
                                                                                                                           half.population.bioassay.survival.resistance = 900),
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = efficacy.j)
f.exposure = seq(0.1, 1, by = 0.1)

the.list = list()
for(i in 1:length(f.exposure)){

  response.i = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.i,
                                                                                                female.fitness.cost = 0,
                                                                                                male.fitness.cost = 0,
                                                                                                female.insecticide.exposure = f.exposure[i],
                                                                                                male.insecticide.exposure = 1, #as is m*f
                                                                                                z.sd.intercept = 18,
                                                                                                z.sd.coefficient = 0.4,
                                                                                                vector.length = 100000,
                                                                                                maximum.bioassay.survival.proportion = 1,
                                                                                                michaelis.menten.slope = 1,
                                                                                                half.population.bioassay.survival.resistance = 900,
                                                                                                regression.coefficient = 0.48,
                                                                                                regression.intercept = 0.15,
                                                                                                current.insecticide.efficacy = efficacy.i,
                                                                                                exposure.scaling.factor = 1,
                                                                                                heritability = 0.2,
                                                                                                survival.to.other.insecticide = j.survival)

  response.j = wrapper_intervention_site_after_selection_deployed_mixtures_truncation_sd_scaled(intervention.before.selection = initial.resistance.j,
                                                                                                female.fitness.cost = 0,
                                                                                                male.fitness.cost = 0,
                                                                                                female.insecticide.exposure = f.exposure[i],
                                                                                                male.insecticide.exposure = 1, #as is m*f
                                                                                                z.sd.intercept = 18,
                                                                                                z.sd.coefficient = 0.4,
                                                                                                vector.length = 100000,
                                                                                                maximum.bioassay.survival.proportion = 1,
                                                                                                michaelis.menten.slope = 1,
                                                                                                half.population.bioassay.survival.resistance = 900,
                                                                                                regression.coefficient = 0.48,
                                                                                                regression.intercept = 0.15,
                                                                                                current.insecticide.efficacy = efficacy.j,
                                                                                                exposure.scaling.factor = 1,
                                                                                                heritability = 0.2,
                                                                                                survival.to.other.insecticide = i.survival)
  #Here 0 is bad. 1 is good.
  proportion.surviving = (((i.survival * j.survival * f.exposure[i])) + (1 - f.exposure[i]))

  start.bioassay.i = (convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.i))*100
  start.bioassay.j = (convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.j))*100
  end.bioassay.i = convert_resistance_score_to_bioassay_survival(trait.mean = response.i)*100
  end.bioassay.j = convert_resistance_score_to_bioassay_survival(trait.mean = response.j)*100
  bioassay.change.i = (end.bioassay.i - start.bioassay.i)
  bioassay.change.j = (end.bioassay.j - start.bioassay.j)
  total.change = bioassay.change.i + bioassay.change.j

  #Here 1 is bad. 0 is good.
  irm.strength = total.change / max(total.change)

  #Composite measure of IRM and Control
  #Here 1 is good ; 0 is bad
  irm.transmission = (proportion.surviving * irm.strength)

  start.bioassay.i = round(convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.i), 2)*100
  start.bioassay.j = round(convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.j), 2)*100
  calc.midpoint = rep(max(total.change)/2, 6084)
  female.exposure = round(rep(c(f.exposure[i]), 6084), 1)


  df = data.frame(proportion.surviving, total.change, bioassay.change.i, bioassay.change.j,
                  start.bioassay.i, start.bioassay.j, efficacy.i,
                  irm.strength, irm.transmission,
                  efficacy.j, female.exposure, calc.midpoint)

  the.list[[i]] = df

}




the.df = do.call(rbind, the.list)


#First do graphs only with insecticide i [insecticide j efficacy == 0]
the.df.i = subset(the.df, efficacy.j == 0 &
                    female.exposure %in% c(0.2, 0.4, 0.6, 0.8, 1))



#figure 1
plot.irm = ggplot(the.df.i, aes(x=efficacy.i,
                                y=bioassay.change.i,
                                group = female.exposure,
                                colour = as.character(female.exposure)))+
  geom_line(linewidth = 1)+
  geom_point()+
  scale_colour_manual(values = c("#1b9e77",
                                 "#d95f02",
                                 "#7570b3",
                                 "#e7298a",
                                 "#66a61e"),
                      name = "Female Exposure")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 1.75, 0.25),
                     limits = c(0, 2.25))+
  xlab("Insecticide Efficacy")+
  ylab(paste0("Single Generation Change\nin Bioassay Survival (%)"))+
  facet_grid( ~ start.bioassay.i)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(colour = "black",
                                   angle = 90))

plot.irm



plot_mixture_efficacy_resistance_space= function(f.exposure){

  plot.i = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                     y=efficacy.j,
                                                                     fill = bioassay.change.i))+
    geom_tile()+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Insecticide i")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black"),
          axis.text.y = element_text(colour = "black"))


  plot.j = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                     y=efficacy.j,
                                                                     fill = bioassay.change.j))+
    geom_tile()+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Insecticide j")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black"),
          axis.text.y = element_text(colour = "black"))


  plot.ij = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                      y=efficacy.j,
                                                                      fill = total.change))+
    geom_tile()+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Total")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black"),
          axis.text.y = element_text(colour = "black"))


  the.legend = cowplot::get_legend(ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                                             y=efficacy.j,
                                                                                             fill = total.change))+
                                     geom_tile()+
                                     scale_fill_gradient2(low = "white",
                                                          mid = "skyblue",
                                                          high = "red",
                                                          midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1],
                                                          name = "Change Bioassay Survival")+
                                     scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                                                            breaks = NULL,
                                                                            labels = NULL),
                                                        expand = c(0,0))+
                                     scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                                                            breaks = NULL,
                                                                            labels = NULL),
                                                        expand = c(0,0))+
                                     facet_grid(start.bioassay.j ~ start.bioassay.i)+
                                     ggtitle("Insecticide i and j")+
                                     theme_classic()+
                                     theme(legend.direction = "horizontal")
  )


  the.layout = "
AAAABBBBCCCC
AAAABBBBCCCC
AAAABBBBCCCC
AAAABBBBCCCC
###DDDDDD###
"


  end.plot = plot.i + plot.j + plot.ij + the.legend +
    plot_layout(design = the.layout) +
    plot_annotation(title = paste0("Insecticide Exposure =", f.exposure))

  return(end.plot)

}


plot_mixture_efficacy_resistance_space(f.exposure = 0.6)

plot_mixture_efficacy_resistance_space_sensitivity= function(f.exposure){


  plot.ij = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                      y=efficacy.j,
                                                                      fill = total.change))+
    geom_tile()+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Total")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black"),
          axis.text.y = element_text(colour = "black"))


  the.legend = cowplot::get_legend(ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                                             y=efficacy.j,
                                                                                             fill = total.change))+
                                     geom_tile()+
                                     scale_fill_gradient2(low = "white",
                                                          mid = "skyblue",
                                                          high = "red",
                                                          midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1],
                                                          name = "Change Bioassay Survival")+
                                     scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                                                            breaks = NULL,
                                                                            labels = NULL),
                                                        expand = c(0,0))+
                                     scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                                                            breaks = NULL,
                                                                            labels = NULL),
                                                        expand = c(0,0))+
                                     facet_grid(start.bioassay.j ~ start.bioassay.i)+
                                     ggtitle("Insecticide i and j")+
                                     theme_classic()+
                                     theme(legend.direction = "horizontal")
  )


  the.layout = "
AAAA
AAAA
AAAA
#BB#
"


  end.plot = plot.ij + the.legend +
    plot_layout(design = the.layout) +
    plot_annotation(title = paste0("Insecticide Exposure =", f.exposure))

  return(end.plot)

}

#Supplementary Figures
plot_mixture_efficacy_resistance_space(f.exposure = 1)


ggsave(
  filename = "chapter4_figureS2.2.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 1200,
  height = 400,
  units = "px",
  dpi = 600)

plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.9)
ggsave(
  filename = "chapter4_figureS2.1a.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)

plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.8)
ggsave(
  filename = "chapter4_figureS2.1b.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.7)
ggsave(
  filename = "chapter4_figureS2.1c.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.5)
ggsave(
  filename = "chapter4_figureS2.1d.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.4)
ggsave(
  filename = "chapter4_figureS2.1e.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.3)
ggsave(
  filename = "chapter4_figureS2.1f.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.2)
ggsave(
  filename = "chapter4_figureS2.1g.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)
plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.1)
ggsave(
  filename = "chapter4_figureS2.1h.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)

plot_mixture_efficacy_resistance_space_sensitivity(f.exposure = 0.6)
ggsave(
  filename = "chapter4_figureS2.1i.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 400,
  units = "px",
  dpi = 600)







plot_mixture_efficacy_resistance_space_novel_j= function(f.exposure){

  temp.df =  subset(the.df, female.exposure == f.exposure &
                      start.bioassay.i == 0 &
                    efficacy.i != 0)

  #split into quantiles
  temp.df$`Selection Rate` = cut(temp.df$bioassay.change.i, 4,
                  labels = c("low", "moderate",
                             "high", "very high"))

  plot.i = ggplot(subset(temp.df,
                         efficacy.i != 0), aes(x=efficacy.i,
                               y=efficacy.j,
                               fill = `Selection Rate`))+
    geom_tile(colour = "black")+
    scale_fill_manual(values = c("blue",#low = blue
                                 "yellow",#moderate = yellow
                                 "orange", # high
                                 "red" #very high =  red
    ))+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = c(0, 0.5, 1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle(paste0("Insecticide Encounter Probability =", f.exposure))+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "right",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black"),
          axis.text.y = element_text(colour = "black"))


  the.table = table(temp.df$start.bioassay.j,
        temp.df$`Selection Rate`)


  end.plot = plot.i
  return(list(end.plot, the.table))

}

plot_mixture_efficacy_resistance_space_novel_j(0.6)


temp.df =  subset(the.df, female.exposure == 0.6 &
                    start.bioassay.i == 0)
temp.df$`Selection Rate` = cut(temp.df$bioassay.change.i, 4,
                               labels = c("low", "moderate",
                                          "high", "very high"))


A = cut(temp.df$bioassay.change.i, 4)
levels(A)



