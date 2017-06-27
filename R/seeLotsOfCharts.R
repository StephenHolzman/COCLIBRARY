seeLotsOfCharts <- function(directory){
  
  #Demo Dodged Bar Chart
  df <- iris %>%
    gather(variable, value, -Species) %>%
    group_by(Species, variable) %>%
    summarise(mean_measurement = mean(value)) %>%
    arrange(variable)
  
  df$variable <- factor(df$variable)
  levels(df$variable) <- rev(levels(df$variable))

  basicBar(
    title = "Mean Values of the Infamous Iris Dataset",
    subtitle = "Demonstrate Vertical Dodged Barchart",
    data = df,
    xvar = "variable",
    yvar = "mean_measurement",
    colourvar = "Species",
    colourvarorder = rev(levels(df$Species)),
    author = "COCLIBRARY DEMO",
    cite = "Source: Iris Dataset",
    flip = FALSE,
    path = paste0(directory,"01-bar-dodge.png")
    )
  basicBar(
    title = "Mean Values of the Infamous Iris Dataset",
    subtitle = "Demonstrate Horizontal Dodged Barchart",
    data = df,
    xvar = "variable",
    yvar = "mean_measurement",
    colourvar = "Species",
    colourvarorder = rev(levels(df$Species)),
    author = "COCLIBRARY DEMO",
    cite = "Source: Iris Dataset",
    flip = TRUE,
    path = paste0(directory,"02-bar-dodge.png")
  )
  #Demo Stacked Bar Chart

  basicBar(
    title = "Mean Values of the Infamous Iris Dataset",
    subtitle = "Demonstrate Vertical Stacked Barchart",
    data = df,
    xvar = "Species",
    yvar = "mean_measurement",
    pos = "stack",
    flip = TRUE,
    colourvar = "variable",
    author = "COCLIBRARY DEMO",
    cite = "Source: Iris Dataset",
    path = paste0(directory,"03-bar-stack-horiz.png")
  )
    
    basicBar(
      title = "Mean Values of the Infamous Iris Dataset",
      subtitle = "Demonstrate Horizontal Stacked Barchart",
      data = df,
      xvar = "Species",
      yvar = "mean_measurement",
      pos = "stack",
      flip = FALSE,
      colourvar = "variable",
      author = "COCLIBRARY DEMO",
      cite = "Source: Iris Dataset",
      path = paste0(directory,"04-bar-stack-vert.png")
    ) 

}
