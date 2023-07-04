library(modelsummary)
show_regressions <- function(regression_list) {
  modelsummary(regression_list,
               shape = "rbind",
               stars = TRUE,
               note = 'Numbers in parentheses represent standard errors. Clustering is indicated in the row "Std.Errors". ',
               coef_rename = TRUE,
               coef_omit = "Intercept",
               gof_omit = "IC|Adj|RMSE",
               align = paste(c(
                 "l", 
                 rep("d", length(regression_list[[1]]))
               ),
               collapse = "")) |> 
    kableExtra::landscape()
}
