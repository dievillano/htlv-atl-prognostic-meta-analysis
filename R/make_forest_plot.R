make_forest_plot <- function(model, data, factor_name, figures_path) {
  
  out_file <- fs::path(figures_path, paste0("forest_", factor_name, ".png"))
  
  png(
    filename = out_file,
    width = 2400,
    height = 1800,
    res = 300
  )
  
  metafor::forest(
    model,
    slab = data$study_label,
    atransf = exp,
    xlab = "Hazard ratio",
    mlab = "Random-effects model",
    header = "Study",
    cex = 0.9
  )
  
  title(
    main = paste0("Forest plot: ", factor_name)
  )
  
  dev.off()
  
  out_file
}

