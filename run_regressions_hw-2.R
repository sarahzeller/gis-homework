run_regressions <- function(dep_var, 
                            indep_vars = explanatory_vals,
                            data = data) {
  # check there's a state variable
  assertthat::assert_that(all(c("state") %in% names(data)),
                          msg = "The data frame needs to include a state and raster_id variable.")
  # check that there's a geometry column
  assertthat::assert_that(c("x","y") %in% names(data),
                          msg = "The data frame needs to include geometry variables x and y.")
  
  assertthat::assert_that(all(is.character(dep_var), is.character(indep_vars)),
                          msg = "The variables need to be input as characters")
  
  formula <- reformulate(indep_vars, dep_var)
  
  baseline <- 
    data |> 
    feols(fml = formula,
          data = _)
  
  fe_state <-
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state")
  
  fe_hetero <-
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state",
          vcov = "hetero")
  
  conley <- 
    data |> 
    feols(fml = formula,
          data = _,
          vcov = "conley")
  
  list <- list(baseline,
               fe_state,
               fe_hetero,
               conley)
  return(list)
}
