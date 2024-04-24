library(devtools)
load_all()
library(patchwork)

#plotting the "additional" kill of mixtures.

#Here we are plotting the impact of insecticide decay
  #on the efficacy of mixtures for IRM

mechanistic_explaination_mixtures = function(){



prs.values = create_normal_distribution(vector.length = 1000,
                                        trait.mean = 100,
                                        standard.deviation = 20)
init.freq = calculate_density_of_trait_values(vector.length = 1000,
                                              trait.mean = 100,
                                              standard.deviation = 20)

avoids.selection = init.freq * 0.3
gets.selected = init.freq * 0.7


med.i.efficacy = c(rep(0, 400), gets.selected[401:1000])
killed.by.i = c(gets.selected[1:400], rep(0, 600))


med.i.high.j = med.i.efficacy * 0.1
med.i.med.j = med.i.efficacy * 0.3
med.i.low.j = med.i.efficacy * 0.7

#End populations:::

med.i.end = med.i.efficacy + avoids.selection
med.i.high.j.end = med.i.high.j + avoids.selection
med.i.med.j.end = med.i.med.j + avoids.selection
med.i.low.j.end = med.i.low.j + avoids.selection




selection.differential.solo = sum((med.i.end)*prs.values)/(sum(med.i.end))
selection.differential.high.j = sum((med.i.high.j.end)*prs.values)/(sum(med.i.high.j.end))
selection.differential.med.j = sum((med.i.med.j.end)*prs.values)/(sum(med.i.med.j.end))
selection.differential.low.j = sum((med.i.low.j.end)*prs.values)/(sum(med.i.low.j.end))

temp.df = data.frame(prs.values,
                     init.freq,
                     avoids.selection,
                     med.i.efficacy,
                     med.i.high.j,
                     med.i.med.j,
                     med.i.low.j,
                     killed.by.i,
                     med.i.end,
                     med.i.high.j.end,
                     med.i.med.j.end,
                     med.i.low.j.end)

#Choose a good/readible colour scheme::

# Colour Scheme:
# Initial emerge:: green  = #addd8e
# Avoids Selection:: Blue = #6baed6
# Killed by i: red = #fb6a4a
# killed by j: orange = #fd8d3c
# Survives i and j : purple = #807dba





initial.population.plot = ggplot(temp.df, aes(x=prs.values,
                                              y=init.freq))+
  geom_area(fill = "#addd8e")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Initial Population")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())



avoids.selection.plot = ggplot(temp.df, aes(x=prs.values,
                                            y=avoids.selection))+
  geom_area(fill = "#6baed6")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Avoids Insecticides")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

solo.med.plot = ggplot(temp.df, aes(x=prs.values,
                                    y=med.i.efficacy))+
  geom_area(aes(x=prs.values, y=gets.selected),
            fill = "#807dba")+
  geom_area(fill = "#807dba")+
  geom_area(aes(x=prs.values, y=killed.by.i),
            fill = "#fb6a4a")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ggtitle("Insecticide i Monotherapy")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


med.i.high.j.plot = ggplot(temp.df, aes(x=prs.values,
                                        y=med.i.high.j))+
  geom_area(aes(x=prs.values, y=gets.selected),
            fill = "#fd8d3c")+ # fills in any gaps
  geom_area(aes(x=prs.values, y=killed.by.i),
            fill = "#fb6a4a")+ #killed by i
  geom_area(aes(x=prs.values, y = med.i.efficacy),
            fill = "#fd8d3c")+ # additional kill
  geom_area(fill = "#807dba")+ #survivors
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ggtitle("High Dose Partner")+
ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

med.i.med.j.plot = ggplot(temp.df, aes(x=prs.values,
                                       y=med.i.med.j))+
  geom_area(aes(x=prs.values, y=gets.selected),
            fill = "#fd8d3c")+ # fills in any gaps
  geom_area(aes(x=prs.values, y=killed.by.i),
            fill = "#fb6a4a")+ #killed by i
  geom_area(aes(x=prs.values, y = med.i.efficacy),
            fill = "#fd8d3c")+ # additional kill
  geom_area(fill = "#807dba")+ #survivors
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ggtitle("Moderate Dose Partner")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

med.i.low.j.plot = ggplot(temp.df, aes(x=prs.values,
                                       y=med.i.low.j))+
  geom_area(aes(x=prs.values, y=gets.selected),
            fill = "#fd8d3c")+ # fills in any gaps
  geom_area(aes(x=prs.values, y=killed.by.i),
            fill = "#fb6a4a")+ #killed by i
  geom_area(aes(x=prs.values, y = med.i.efficacy),
            fill = "#fd8d3c")+ # additional kill
  geom_area(fill = "#807dba")+ #survivors
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ggtitle("Low Dose Partner")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

####End Distributions:::

end.solo = ggplot(temp.df, aes(x=prs.values,
                    y=med.i.end))+
  geom_area(fill = "#807dba")+ # fills in any gaps
  geom_area(aes(x=prs.values, y=avoids.selection),
            fill = "#6baed6")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = selection.differential.solo,
             linetype = "dashed",
             colour = "#ce1256")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Parental Population")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


end.high.j = ggplot(temp.df, aes(x=prs.values,
                    y=med.i.high.j.end))+
  geom_area(fill = "#807dba")+ # fills in any gaps
  geom_area(aes(x= prs.values, y = avoids.selection),
            fill = "#6baed6")+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = selection.differential.high.j,
             linetype = "dashed",
             colour = "#ce1256")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

end.med.j = ggplot(temp.df, aes(x=prs.values,
                    y=med.i.med.j.end))+
  geom_area(fill = "#807dba")+ # fills in any gaps
  geom_area(aes(x= prs.values, y = avoids.selection),
            fill = "#6baed6")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = selection.differential.med.j,
             linetype = "dashed",
             colour = "#ce1256")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


end.low.j = ggplot(temp.df, aes(x=prs.values,
                    y=med.i.low.j.end))+
  geom_area(fill = "#807dba")+ # fills in any gaps
  geom_area(aes(x= prs.values, y = avoids.selection),
            fill = "#6baed6")+
  ylim(0, 0.02)+#
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = selection.differential.low.j,
             linetype = "dashed",
             colour = "#ce1256")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


the.layout = "
#B#
#CG
ADH
#EI
#FJ
"

final.plot = initial.population.plot +
  avoids.selection.plot+
  solo.med.plot+
  med.i.low.j.plot +
  med.i.med.j.plot +
  med.i.high.j.plot+
  end.solo +
  end.low.j +
  end.med.j+
  end.high.j+
  plot_layout(design = the.layout)

return(final.plot)
}


mechanistic_explaination_mixtures()





































