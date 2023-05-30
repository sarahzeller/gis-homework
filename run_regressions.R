run_regressions <- function(data, formula) {
  # check there's a state variable
  assertthat::assert_that("state" %in% names(data),
                          msg = "The data frame needs to include a state variable.")
  # check that there's a geometry column
  assertthat::assert_that("geometry" %in% names(data),
                          msg = "The data frame needs to include a geometry variable.")
  
  # convert geometry to latitude/longitude for conley SE
  data_w_lat_lon <- 
    data |> 
    st_transform(4326) |> 
    extract(geometry, 
            c('lat', 'lon'), 
            '\\((.*), (.*)\\)', 
            convert = TRUE)
    
  baseline <- 
    data |> 
    feols(fml = formula,
          data = _)
  
  fe <-
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state")
  
  fe_cluster_se <- 
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state",
          se = "cluster")
  
  fe_hetero <-
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state",
          vcov = "hetero")
  
  conley <- 
    data_w_lat_lon |> 
    feols(fml = formula,
          data = _,
          vcov = "conley")
  
  list <- list(baseline,
               fe,
               fe_cluster_se,
               fe_hetero,
               conley)
  return(list)
}