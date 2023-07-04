run_regressions <- function(dep_var,
                            indep_vars = explanatory_vals,
                            dataset = data,
                            conley = FALSE) {
  # check there's a state variable
  assertthat::assert_that(all(c("state") %in% names(data)),
                          msg = "The data frame needs to include a state variable.")
  # check that there's a geometry column
  assertthat::assert_that(all(c("lon", "lat") %in% names(dataset)),
                          msg = "The data frame needs to include geometry variables x and y.")
  
  assertthat::assert_that(all(is.character(dep_var), is.character(indep_vars)),
                          msg = "The variables need to be input as characters")
  
  formula <- reformulate(indep_vars, dep_var)
  
  baseline <-
    dataset |>
    feols(fml = formula,
          data = _)
  
  fe_state <-
    dataset |>
    feols(fml = formula,
          data = _,
          fixef = "state")
  
  fe_hetero <-
    dataset |>
    feols(
      fml = formula,
      data = _,
      fixef = "state",
      vcov = "hetero"
    )
  
  # only add Conley regression if needed (takes forever)
  if (conley == FALSE) {
    list <- list(baseline,
                 fe_state,
                 fe_hetero)
  } else if (conley == TRUE) {
    conley <-
      dataset |>
      feols(fml = formula,
            data = _,
            vcov = "conley")
    
    list <- list(baseline,
                 fe_state,
                 fe_hetero,
                 conley)
  }
  
  return(list)
}
