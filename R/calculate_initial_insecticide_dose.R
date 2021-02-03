#' @title Calculate the Relative Dose of the Initial Application
#'
#' @param applied.dose
#' @param recommended.dose

#This is equations 1b(iii) and 1b(iv) in base_model v2
calculate_initial_insecticide_dose = function(applied.dose,
                                              recommended.dose){

  relative.dose = applied.dose / recommended.dose

    return(relative.dose)

}
