#' @title Plot the relationship between resistance and survival calculated from insecticide resistance
#' @import ggplot2
#'
#' @export
#'
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param bioassay.survival The survival that occured in the bioassay, as a proportion(values must be between 0 and 1). Where 1=all survived, 0=all died
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param sd.population.resistance How much variation in the population resistance.
#' @param nsim How many replications of the rnorm function are conducted. Recommended value is 1000.
#' @param minimum.resistance Recommend setting as 0.
#' @param maximum.resistance This will depend on the half survival scale, but 10000 would be a good start.
#'
#' @return A ggplot of the relationship between resistance (x axis) and survival (y axis).


plot_resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                half.population.bioassay.survival.resistance = 900,
                                                minimum.resistance = 0,
                                                maximum.resistance = 25000,
                                                resistance.score.divisions = 1){

  resistance.scores=seq(minimum.resistance, maximum.resistance, by = resistance.score.divisions)

  bioassay.survival.proportion = c()

  for(i in 1:length(resistance.scores)){

    bioassay.survival.proportion[i] = convert_resistance_score_to_bioassay_survival(
      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
      trait.mean = resistance.scores[i],
      michaelis.menten.slope=michaelis.menten.slope,
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance)
  }


  df=data.frame(resistance.scores, bioassay.survival.proportion)

  ggplot(df, aes(x=resistance.scores, y = bioassay.survival.proportion)) + ##logging to increase ease of readibility
    geom_point(colour = "blue") +
    xlab("Polygenic Resistance Score") + ##applying log(z) makes it more readable at the lower z levels.
    ylab("WHO Cylinder Survival Proportion")+ #note: this label may need to be changed depending on what we calibrate against
    theme_classic()
}

#Note: This is being really slow at plotting.
