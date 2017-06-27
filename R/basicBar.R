#Plotting
basicBar <- function(data, xvar, yvar,positvar=NULL,labelvar = NULL,showfooter=TRUE,labelcol="white",extramargins = c(0,0,0,0), xvarorder = NULL, valuelabels = TRUE, facetvar = NULL, colourvar = NULL,colourvarorder = NULL, stat = "identity", pos = "dodge", flip = TRUE, title = "", subtitle = "", legendtitle = NULL, cite = "", author = "", path = NULL,  xtitle = NULL, xlimits = NULL, xbreaks = NULL, xlabels = NULL, ytitle = NULL, ylimits = NULL, ybreaks = NULL, ylabels = NULL, colpal = styling$colors$main, width = 800, height = 500, styling = coc_styling) {
  
  if(is.vector(xvarorder)){
    data[[xvar]] <- as.character(data[[xvar]])
    data[[xvar]] <- factor(data[[xvar]],levels =xvarorder)
  }else{
    #levels(data[[xvar]]) <- rev(levels(data[[xvar]]))
  }
  if(is.vector(colourvarorder)){
    data[[colourvar]] <- as.character(data[[colourvar]])
    data[[colourvar]] <- factor(data[[colourvar]],levels =colourvarorder)
  }else{
    levels(data[[colourvar]]) <- (levels(data[[colourvar]]))
  }

  if(is.character(colourvar)){
    p <- ggplot(data, aes_string(x = xvar, y = yvar, fill = colourvar))
    p <- p + geom_bar(stat = stat, position = pos, alpha = 1)
    p <- p + scale_fill_manual(values = colpal)
  }else{
    p <- ggplot(data, aes_string(x = xvar, y = yvar))
    p <- p + geom_bar(stat = stat, fill = colpal[1], position = pos, alpha = 1)
  }
  
  if((flip && pos == "stack") | (!flip && pos == "dodge")){
    legendorientation <- "top"
    if(pos == "stack"){
      p <- p + guides(fill = guide_legend(reverse = TRUE))
    }else{
      p <- p + guides(fill = guide_legend(reverse = FALSE))
    }    
    p <- p + theme(plot.margin = unit(c(70 + extramargins[1], 20 + extramargins[2], 50 + extramargins[3], 20 + extramargins[4]),"points"),
                   axis.title.y = element_text(size = styling$axis$labels$font$size, angle = 90,margin=margin(0,20,0,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
                   axis.title.x = element_text(size = styling$axis$labels$font$size,margin=margin(20,0,10,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color)
    )
  }else{
    legendorientation <- "right"
    if(pos == "stack"){
      p <- p + guides(fill = guide_legend(reverse = FALSE))
    }else{
      p <- p + guides(fill = guide_legend(reverse = TRUE))
    }
    p <- p + theme(plot.margin = unit(c(90+extramargins[1], 10 + extramargins[2], 50 + extramargins[3], 20 + extramargins[4]),"points"),
                   axis.title.y = element_text(size = styling$axis$labels$font$size, angle = 90,margin=margin(0,20,0,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
                   axis.title.x = element_text(size = styling$axis$labels$font$size,margin=margin(20,0,10,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color)
    )
    
  }
  

  p <- p + theme(
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    axis.text = element_text(size = styling$axis$labels$font$size, family = styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
    axis.ticks = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.line = element_line(colour = "black", size = 1.5),
    panel.background = element_rect(fill = styling$margins$color, colour = styling$plot$color),
    legend.key.height = unit(25,"points"),
    legend.key.width = unit(25,"points"),
    legend.key = element_rect(fill = styling$legend$color, colour = styling$legend$color),
    legend.title = element_text(size = styling$legend$title$font$size,margin=margin(0,0,0,0),family=styling$legend$title$font$family,face=styling$legend$title$font$face),
    legend.position = legendorientation,
    legend.text = element_text(family=styling$legend$labels$font$family,size = 20,colour = styling$legend$labels$font$color),
    legend.background = element_rect(fill = styling$legend$color, colour = styling$legend$color),
    plot.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),
    strip.background = element_rect(fill = styling$facet$labels$color, colour = styling$facet$labels$color),
    strip.text = element_text(colour=styling$facet$labels$font$color,size=styling$facet$labels$font$size,family=styling$facet$labels$font$family)
    )

  if(flip){
    p <- p + theme(
      panel.grid.major.x = element_line(colour = styling$grid$lines$color),
      panel.grid.major.y = element_blank()
      )
   }else{
    p <- p + theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = styling$grid$lines$color)
      )    
  }

  
  if(is.vector(ybreaks)){
    if(is.vector(ylabels)){
      p <- p + scale_y_continuous(limits = ylimits,breaks = ybreaks, labels = ylabels, expand = c(0,0))
    }else{
      p <- p + scale_y_continuous(limits = ylimits,breaks = ybreaks, expand = c(0,0))
    }
  }
  #data <- mutate(data,positval = cumsum(value) - .5*value)
  
  if((is.character(positvar) && pos == "dodge")){
    
    p <- p + geom_text(aes_string(label=labelvar,y=positvar),position=position_dodge(width=.9),colour=labelcol,size=8,family=styling$legend$labels$font$family)
  }else if(is.character(positvar)){
    p <- p + geom_text(aes_string(label=labelvar,y=positvar),colour=labelcol,size=8,family=styling$legend$labels$font$family)
    
  }
  
  if(is.vector(xlabels)){
    p <- p + scale_x_discrete(labels = xlabels)
  }
  p <- p + xlab(xtitle)
  p <- p + ylab(ytitle)
  p <- p + labs(fill = legendtitle)
  p <- p + guides(col = guide_legend(override.aes = list(shape = 15, size = 10)))
  if(is.character(facetvar)){
    p <- p + facet_grid(reformulate(facetvar))
  }
  if(flip){
    p <- p + coord_flip()   
  }
  #if(valuelabels){
  #}
  
  #Save to PNG
  if(is.character(path)){
    if(substr(path,nchar(path)-2,nchar(path))=="png"){
      savePNG(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, styling = styling,showfooter=showfooter)
    }else if(substr(path,nchar(path)-2,nchar(path))=="pdf"){
      savePDF(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, styling = styling)
    }    
  }else{
    print(p)
  }

}