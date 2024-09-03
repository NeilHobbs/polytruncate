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
the.df.i = subset(the.df, efficacy.j == 0)
                  # & female.exposure %in% c(0.2, 0.4, 0.6, 0.8, 1))

RColorBrewer::brewer.pal(name = "Purples", n = 10)

#figure 1
plot.irm = ggplot(the.df.i, aes(x=efficacy.i,
                                y=bioassay.change.i,
                                group = female.exposure,
                                colour = as.character(female.exposure)))+
  geom_line(linewidth = 1.5)+
  # geom_point()+
  # scale_colour_manual(values = c("#1b9e77",
  #                                "#d95f02",
  #                                "#7570b3",
  #                                "#e7298a",
  #                                "#66a61e"),
  #                     name = "Female Exposure")+

  scale_colour_viridis_d(option = "plasma", direction = -1,
                         name = "Insecticide Exposure")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(seq(0, 1.1, 0.1)),
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 2.25, 0.25),
                     limits = c(0, 2.25))+
  xlab("Insecticide Efficacy")+
  ylab(paste0("Absolute Single Generation Increase\nin Bioassay Survival (%)"))+
  facet_grid( ~ start.bioassay.i)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 90,
                                   vjust = 0.5),
        strip.background = element_rect(fill = "white"))

plot.irm


ggsave(filename = "monotherapy_insecticide_decay_response.jpeg",
       plot = last_plot(),
       dpi = 600,
       height = 800,
       width = 1200,
       scale = 5,
       units = "px")




plot.control = ggplot(the.df.i, aes(x=efficacy.i,
                                y=1-proportion.surviving,
                                group = female.exposure,
                                colour = as.character(female.exposure)))+
  geom_line(linewidth = 1.5)+
  # geom_point()+
  # scale_colour_manual(values = c("#1b9e77",
  #                                "#d95f02",
  #                                "#7570b3",
  #                                "#e7298a",
  #                                "#66a61e"),
  #                     name = "Female Exposure")+

  scale_colour_viridis_d(option = "plasma", direction = -1,
                         name = "Insecticide Exposure")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(seq(0, 1.1, 0.1)),
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 1, 0.2),
                     limits = c(0, 1))+
  xlab("Insecticide Efficacy")+
  ylab(paste0("Degree of Control"))+
  facet_grid( ~ start.bioassay.i)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 90,
                                   vjust = 0.5),
        strip.background = element_rect(fill = "white"))


plot.control

ggsave(filename = "monotherapy_insecticide_decay_control.jpeg",
       plot = last_plot(),
       dpi = 600,
       height = 800,
       width = 1200,
       scale = 5,
       units = "px")








plot_mixture_efficacy_resistance_space= function(f.exposure){

  plot.i = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                     y=efficacy.j,
                                                                     fill = bioassay.change.i))+
    geom_tile(colour = "grey")+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Insecticide i")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black",
                                     vjust = 0.5,
                                     size = 4.3),
          axis.text.y = element_text(colour = "black",
                                     size = 4.5))


  plot.j = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                     y=efficacy.j,
                                                                     fill = bioassay.change.j))+
    geom_tile(colour = "grey")+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Insecticide j")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black",
                                     vjust = 0.5,
                                     size = 4.3),
          axis.text.y = element_text(colour = "black",
                                     size = 4.5))


  plot.ij = ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                      y=efficacy.j,
                                                                      fill = total.change))+
    geom_tile(colour = "grey")+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1])+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle("Total")+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black",
                                     vjust = 0.5,
                                     size = 4.3),
          axis.text.y = element_text(colour = "black",
                                     size = 4.5))


  the.legend = cowplot::get_legend(ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                                             y=efficacy.j,
                                                                                             fill = total.change))+
                                     geom_tile()+
                                     scale_fill_gradient2(low = "white",
                                                          mid = "skyblue",
                                                          high = "red",
                                                          midpoint = subset(the.df, female.exposure == f.exposure)$calc.midpoint[1],
                                                          name = "Absolute Single\nGeneration Increase in\nBioassay Survival (%)")+
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
                                     theme(legend.direction = "horizontal",
                                           legend.background = element_rect(colour = "black"))
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
    plot_annotation(title = paste0("Insecticide Exposure = ", f.exposure))

  return(end.plot)

}

plot_mixture_efficacy_resistance_space(0.6)

ggsave(plot = last_plot(),
       filename = paste0("Mixture_efficacy_space_response.0.6.jpeg"),
       dpi = 600,
       height = 800,
       width = 1800,
       scale = 5,
       units = "px")


# exposure = seq(0.1, 1, 0.1)
# for(i in 1:10){
#
#   ggsave(plot = plot_mixture_efficacy_resistance_space(exposure[i]),
#        filename = paste0("Mixture_efficacy_space_response",exposure[i],".jpeg"),
#        dpi = 600,
#        height = 800,
#        width = 1800,
#        scale = 5,
#        units = "px")
#
#   print(i)
# }


plot_mixture_efficacy_resistance_space= function(i){

  plot.ij = ggplot(subset(the.df, female.exposure == unique(the.df$female.exposure)[i]), aes(x=efficacy.i,
                                                                      y=efficacy.j,
                                                                      fill = total.change))+
    geom_tile(colour = "grey")+
    scale_fill_gradient2(low = "white",
                         mid = "skyblue",
                         high = "red",
                         midpoint = 1)+

    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0,0),
                       breaks = seq(0, 1.2, 0.1))+
    xlab("Efficacy of Insecticide i")+
    ylab("Efficacy of Insecticide j")+
    ggtitle(paste0("Insecticide Exposure = ", unique(the.df$female.exposure)[i], "\nTotal"))+
    facet_grid(start.bioassay.j ~ start.bioassay.i)+
    theme_classic()+
    theme(legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          axis.text.x = element_text(angle = 90,
                                     colour = "black",
                                     vjust = 0.5,
                                     size = 4.3),
          axis.text.y = element_text(colour = "black",
                                     size = 4.5))



  return(plot.ij)

}


plot.list = list()
for(i in 1:10){

plot.list[[i]] = plot_mixture_efficacy_resistance_space(i)

}


the.legend = cowplot::get_legend(ggplot(subset(the.df, female.exposure == f.exposure), aes(x=efficacy.i,
                                                                                           y=efficacy.j,
                                                                                           fill = total.change))+
                                   geom_tile()+
                                   scale_fill_gradient2(low = "white",
                                                        mid = "skyblue",
                                                        high = "red",
                                                        midpoint = 1,
                                                        limits = c(0, max(the.df$total.change)),
                                                           name = "Absolute Single\nGeneration Increase in\nBioassay Survival (%)")+
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
                                   theme(legend.direction = "horizontal",
                                         legend.background = element_rect(colour = "black")))





the.layout = "
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
########KKKK########"


ggsave(plot = plot.list[[1]] +
         plot.list[[2]] +
         plot.list[[3]] +
         plot.list[[4]] +
         plot.list[[5]] +
         plot.list[[6]] +
         plot.list[[7]] +
         plot.list[[8]] +
         plot.list[[9]] +
         plot.list[[10]] +
         the.legend + plot_layout(design = the.layout),
       filename = "Mixture_efficacy_space_resistance_management.jpeg",
       dpi = 600,
       height = 1200,
       width = 2400,
       scale = 5,
       units = "px")


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


start.bioassay.i = round(convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.i), 2)*100
start.bioassay.j = round(convert_resistance_score_to_bioassay_survival(trait.mean = initial.resistance.j), 2)*100

overall.survival = i.survival * j.survival



f.exposure = seq(0.1, 1, by = 0.1)


pop.control.list = list()
for(i in 1:10){

  population.control = 1 -  calculate_female_population_size_after_selection_truncation(female.population.size.unexposed = 1 - f.exposure[i],
                                                              female.population.size.exposed.survivors = f.exposure[i] * overall.survival)

pop.control.list[[i]] = data.frame(i.survival, j.survival, efficacy.i, efficacy.j, population.control, exposure = rep(f.exposure[i], length(overall.survival)),
                                   start.bioassay.i, start.bioassay.j)

}

population.control.df = do.call(rbind, pop.control.list)




for(i in 1:10){

  ggsave(plot = ggplot(subset(population.control.df, exposure == unique(population.control.df$exposure)[[i]]), aes(x=efficacy.i,
                                                                                                                   y=efficacy.j,
                                                                                                                   fill = population.control))+
           geom_tile(colour = "grey")+
           scale_fill_viridis_c(limits = c(0, 1),
                                breaks = seq(0, 1, 0.2),
                                name = "Population Control")+
           scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                                  breaks = NULL,
                                                  labels = NULL),
                              expand = c(0,0),
                              breaks = seq(0, 1.2, 0.1))+
           scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                                  breaks = NULL,
                                                  labels = NULL),
                              expand = c(0,0),
                              breaks = seq(0, 1.2, 0.1))+
           xlab("Efficacy of Insecticide i")+
           ylab("Efficacy of Insecticide j")+
           ggtitle(paste0("Exposure = ", unique(population.control.df$exposure)[[i]]))+
           facet_grid(start.bioassay.j ~ start.bioassay.i)+
           theme_classic()+
           theme(legend.position = "bottom",
                 panel.spacing = unit(0.2, "lines"),
                 axis.text.x = element_text(angle = 90,
                                            colour = "black",
                                            vjust = 0.5,
                                            size = 4.3),
                 axis.text.y = element_text(colour = "black",
                                            size = 4.5)),

  filename = paste0("Mixture_efficacy_space_population_control",f.exposure[i],".jpeg"),
  dpi = 600,
  height = 800,
  width = 800,
  scale = 5,
  units = "px")

}



plot.list = list()
for(i in 1:10){
plot.list[[i]] = ggplot(subset(population.control.df, exposure == unique(population.control.df$exposure)[[i]]), aes(x=efficacy.i,
                                                                                                   y=efficacy.j,
                                                                                                   fill = population.control))+
  geom_tile(colour = "grey")+
  scale_fill_viridis_c(limits = c(0, 1),
                       breaks = seq(0, 1, 0.2),
                       name = "Population Control")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0),
                     breaks = seq(0, 1.2, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0),
                     breaks = seq(0, 1.2, 0.1))+
  xlab("Efficacy of Insecticide i")+
  ylab("Efficacy of Insecticide j")+
  ggtitle(paste0("Exposure = ", unique(population.control.df$exposure)[[i]]))+
  facet_grid(start.bioassay.j ~ start.bioassay.i)+
  theme_classic()+
  theme(legend.position = "none",
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(angle = 90,
                                   colour = "black",
                                   vjust = 0.5,
                                   size = 4.3),
        axis.text.y = element_text(colour = "black",
                                   size = 4.5))

}

the.legend = cowplot::get_plot_component(ggplot(subset(population.control.df, exposure == unique(population.control.df$exposure)[[1]]), aes(x=efficacy.i,
                                                                                                                       y=efficacy.j,
                                                                                                                       fill = population.control))+
                      geom_tile(colour = "grey")+
                        scale_fill_viridis_c(limits = c(0, 1),
                                             breaks = seq(0, 1, 0.2),
                                             name = "Degree of Control")+
                      scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide i",
                                                             breaks = NULL,
                                                             labels = NULL),
                                         expand = c(0,0),
                                         breaks = seq(0, 1.2, 0.1))+
                      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Bioassay Surival (%) Insecticide j",
                                                             breaks = NULL,
                                                             labels = NULL),
                                         expand = c(0,0),
                                         breaks = seq(0, 1.2, 0.1))+
                      xlab("Efficacy of Insecticide i")+
                      ylab("Efficacy of Insecticide j")+
                      ggtitle(paste0("Exposure = ", unique(population.control.df$exposure)[[1]]))+
                      facet_grid(start.bioassay.j ~ start.bioassay.i)+
                      theme_classic()+
                      theme(legend.position = "bottom",
                            panel.spacing = unit(0.2, "lines"),
                            axis.text.x = element_text(angle = 90,
                                                       colour = "black",
                                                       vjust = 0.5,
                                                       size = 4.3),
                            axis.text.y = element_text(colour = "black",
                                                       size = 4.5)), "guide-box-bottom", return_all = TRUE)


the.layout = "
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
AAAABBBBCCCCDDDDEEEE
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
FFFFGGGGHHHHIIIIJJJJ
########KKKK########"

plot.list[[1]] +
plot.list[[2]] +
plot.list[[3]] +
plot.list[[4]] +
plot.list[[5]] +
plot.list[[6]] +
plot.list[[7]] +
plot.list[[8]] +
plot.list[[9]] +
plot.list[[10]] +
  the.legend + plot_layout(design = the.layout)




ggsave(plot = last_plot(),
       filename = "Mixture_efficacy_space_population_control.jpeg",
       dpi = 600,
       height = 1200,
       width = 2400,
       scale = 5,
       units = "px")



