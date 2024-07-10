library(devtools)
load_all()
library(patchwork)



mechanistic_explaination_monotherapies = function(){

trait.values = create_normal_distribution(vector.length = 1000,
                                          trait.mean = 0,
                                          standard.deviation = 20)

values.frequency = calculate_density_of_trait_values(vector.length = 1000,
                                                     trait.mean = 0,
                                                     standard.deviation = 20)


unexposed.individuals = 0.3 * values.frequency
exposed.individuals = 0.7 * values.frequency


exposed.survivors.vhigh.efficacy = c(rep(0, 950), values.frequency[951:1000]* 0.7)
exposed.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7)
exposed.survivors.med.efficacy = c(rep(0, 500), values.frequency[501:1000]* 0.7)
exposed.survivors.low.efficacy = c(rep(0, 200), values.frequency[201:1000]* 0.7)

end.survivors.vhigh.efficacy = c(rep(0, 950), values.frequency[951:1000] * 0.7) + unexposed.individuals
end.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7) + unexposed.individuals
end.survivors.med.efficacy = c(rep(0, 500), values.frequency[501:1000]* 0.7) + unexposed.individuals
end.survivors.low.efficacy = c(rep(0, 200), values.frequency[201:1000]* 0.7) + unexposed.individuals



sum(trait.values * end.survivors.med.efficacy)/sum(end.survivors.med.efficacy)
sum(trait.values * end.survivors.high.efficacy)/sum(end.survivors.high.efficacy)



df = data.frame(exposed.survivors.vhigh.efficacy,
                exposed.survivors.high.efficacy,
                exposed.survivors.med.efficacy,
                exposed.survivors.low.efficacy,
                end.survivors.vhigh.efficacy,
                end.survivors.high.efficacy,
                end.survivors.med.efficacy,
                end.survivors.low.efficacy,
                unexposed.individuals,
                exposed.individuals,
                trait.values)


# Colour Scheme:
# Initial emerge:: green  = #addd8e
# Avoids Selection:: Blue = #6baed6
# Killed by i: red = #fb6a4a
# Survives i : purple = #807dba

before.selection = ggplot(df, aes(x=trait.values,
                                  y=values.frequency))+
  geom_area(fill = "#addd8e")+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 2,
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Initial Population")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

unexposed.plot = ggplot(df, aes(x=trait.values,
                                y=unexposed.individuals))+
  geom_area(fill = "#6baed6")+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 2,
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Avoids Insecticides")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
       axis.title = element_blank())


low.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.low.efficacy))+
  geom_area(aes(x=trait.values, y=exposed.individuals),
            fill = "#fb6a4a")+
  geom_area(fill = "#807dba")+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 2)+
  ggtitle("Low Dose Insecticide")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())


low.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.low.efficacy))+
  geom_area(fill = "#807dba")+
  geom_area(aes(x=trait.values, y = unexposed.individuals),
                  fill = "#6baed6")+
    ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 2)+
  geom_vline(xintercept = (sum(trait.values * end.survivors.low.efficacy)/sum(end.survivors.low.efficacy)),
             colour = "#ce1256",
             linetype = "dashed", linewidth = 2)+
  ggtitle("Parental Population")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())

med.efficacy.plot = ggplot(df, aes(x=trait.values,
                                   y=exposed.survivors.med.efficacy))+
  geom_area(aes(x=trait.values, y=exposed.individuals),
            fill = "#fb6a4a")+
  geom_area(fill = "#807dba")+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 2)+
  ggtitle("Moderate Dose Insecticide")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())

med.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                       y=end.survivors.med.efficacy))+
  geom_area(fill = "#807dba")+
  geom_area(aes(x=trait.values, y = unexposed.individuals),
            fill = "#6baed6")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 2)+
  geom_vline(xintercept = (sum(trait.values * end.survivors.med.efficacy)/sum(end.survivors.med.efficacy)),
             colour = "#ce1256",
             linetype = "dashed", linewidth = 2)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())

vhigh.efficacy.plot = ggplot(df, aes(x=trait.values,
                                     y=exposed.survivors.vhigh.efficacy))+
  geom_area(aes(x=trait.values, y=exposed.individuals),
            fill = "#fb6a4a")+
  geom_area(fill = "#807dba")+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 2)+
  ggtitle("High Dose Insecticide")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())


vhigh.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                         y=end.survivors.vhigh.efficacy))+
  geom_area(fill = "#807dba")+
  geom_area(aes(x=trait.values, y = unexposed.individuals),
            fill = "#6baed6")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 2)+
  geom_vline(xintercept = (sum(trait.values * end.survivors.vhigh.efficacy)/sum(end.survivors.vhigh.efficacy)),
             colour = "#ce1256",
             linetype = "dashed", linewidth = 2)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())



legend.df = data.frame(outcome = c("Avoids Insecticide",
                                   "Killed by Insecticide",
                                   "Survives Insecticide"),
                       yvals = 1:3)


legend.df = ggplot(legend.df, aes(x= 1, y = yvals, fill = outcome))+
  geom_tile()+
  geom_text(aes(label = outcome), size = 12)+
  scale_fill_manual(values = c("#6baed6", "#fb6a4a", "#807dba"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


#Add in the  mixtures:
#low-low
#low-med
#low-high
#med-low
#med-med
#med-hihg
#high-low
#high-med
#high-high

the.layout = "
#B#
ACD
#EF
GHI
"


final.plot = before.selection +
  unexposed.plot +
  low.efficacy.plot +
  low.efficacy.end.plot +
  med.efficacy.plot +
  med.efficacy.end.plot +
  legend.df +
  vhigh.efficacy.plot +
  vhigh.efficacy.end.plot +
  plot_layout(design = the.layout)

return(final.plot)
}


mechanistic_explaination_monotherapies()



# high.efficacy.plot = ggplot(df, aes(x=trait.values,
#                                     y=exposed.survivors.high.efficacy))+
#   geom_area(aes(x=trait.values, y=exposed.individuals),
#             fill = "#fb6a4a")+
#   geom_area(fill = "#807dba")+
#   geom_vline(xintercept = 0,
#              colour = "black",
#              linetype = "dashed")+
#   ylim(0, 0.02)+
#   theme_classic()+
#   theme(axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title = element_blank())
#
# high.efficacy.end.plot = ggplot(df, aes(x=trait.values,
#                                         y=end.survivors.high.efficacy))+
#   geom_area(fill = "#807dba")+
#   geom_area(aes(x=trait.values, y = unexposed.individuals),
#             fill = "#6baed6")+
#   ylim(0, 0.02)+
#   geom_vline(xintercept = 0,
#              linetype = "dashed")+
#   geom_vline(xintercept = (sum(trait.values * end.survivors.high.efficacy)/sum(end.survivors.high.efficacy)),
#              colour = "#ce1256",
#              linetype = "dashed")+
#   theme_bw()+
#   theme(axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title = element_blank())
