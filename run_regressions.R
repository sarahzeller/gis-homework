run_regressions <- function(data, formula) {
  # check there's a state variable
  assertthat::assert_that(all(c("state","raster_id") %in% names(data)),
                          msg = "The data frame needs to include a state and raster_id variable.")
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
  
  fe_state <-
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "state")
  
  fe_raster <- 
    data |> 
    feols(fml = formula,
          data = _,
          fixef = "raster_id")
  
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
               fe_state,
               fe_hetero,
               fe_raster,
               conley)
  return(list)
}
