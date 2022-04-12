#'@title Standard Deviation Changes with Population Mean
#'
#'@Description Allows the standard devation of the polygenic resistance score to change in response to the mean
#' polygenic resistance score. Parameter estimates were calculate from field conducted WHO bioassays and then a linear
#' model was conducted on the relationship between the mean score and the standard deviation.

#'@param current.z = The current mean polygenic resistance score of the population.
#'@param z.sd.intercept = The intercept of the linear model between z and sd
#'@param z.sd.coefficient = The regression coefficient of the linear model between z and sd


sd_changes_with_z = function(current.z,
                             z.sd.intercept,
                             z.sd.coefficient){


  current.sd = (current.z*z.sd.coefficient) + z.sd.intercept

}
