#' @title Calculate the Relative Dose of the Initial Application
#'
#' @param applied.dose = The efficacy of the applied insecticide dose.
#' @param recommended.dose = The recommended efficacy of insecticide dose. Should normally be set at 1.

#This is equations 1b(iii) and 1b(iv) in base_model v2
calculate_initial_insecticide_dose = function(applied.dose,
                                              recommended.dose){

  relative.dose = applied.dose / recommended.dose

    return(relative.dose)

}
