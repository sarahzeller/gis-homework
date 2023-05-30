library(modelsummary)
show_regressions <- function(regression_list) {
  modelsummary(regression_list,
               stars = TRUE,
               coef_rename = TRUE,
               coef_omit = "Intercept",
               gof_omit = "IC|Adj|RMSE",
               align = paste(c(
                 "l", 
                 rep("d", length(regression_list))
                 ),
                              collapse = "")) |> 
    landscape()
}
