#Plotting
library(ggplot2)
basicMultiline <- function(data, xvar, yvar, colourvar = NULL, extramargins = c(0,0,0,0),showfooter=TRUE,stat = "identity", pos = "dodge", flip = TRUE, title = "", subtitle = "", legendtitle = "", cite = "", author = "", path = NULL,  xtitle = "", xlimits = NULL, xbreaks = NULL, xlabels = NULL, ytitle = NULL, ylimits = NULL, ybreaks = NULL, ylabels = NULL, colpal = coc_styling$colors$main, width = 800, height = 500, pointsize = 4, plotbackground = 'white',  headerbackground = "#2E2E2E", headerfontcol = "white", footerbackground = "#2E2E2E", footerfontcol = "white", fontfamily = "Arial",styling=coc_styling) {

  if(is.character(colourvar)){
    p <- ggplot(data, aes_string(x = xvar, y = yvar, colour = colourvar, fill = NULL))

    p <- p + scale_colour_manual(values = colpal)
    p <- p + geom_line(size=2)
  }else{
    p <- ggplot(data, aes_string(x = xvar, y = yvar))
    p <- p + scale_colour_manual(values = styling)
    p <- p + geom_line(size=2,colour=colpal[1])
    
  }

  p <- p + theme(panel.grid.major.x = element_line(colour = coc_styling$grid$lines$color),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(colour = coc_styling$grid$lines$color),
                 plot.margin = unit(c(7, 2.5, 4, 2), "lines"),
                 axis.text = element_text(size = styling$axis$labels$font$size, family = styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
                 axis.ticks = element_line(colour = NULL),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.line = element_line(colour = "black", size = 1.5),
                 axis.line.y = element_blank(),
                axis.title.y = element_text(size = rel(1.8), angle = 90,colour=styling$axis$labels$font$color,margin=margin(0,20,0,0),family=fontfamily,face="bold"),
                 axis.title.x = element_text(size = rel(1.8),margin=margin(20,0,0,0),colour=styling$axis$labels$font$color,family=fontfamily,face="bold"),
                 panel.background = element_rect(fill = coc_styling$plot$color, colour = coc_styling$plot$color),
                 legend.key.size = unit(.05,"npc"),
                 legend.key = element_rect(fill = coc_styling$legend$color, colour = coc_styling$legend$color),
                   #legend.title = element_text(size = rel(1.8),margin=margin(0,20,0,50),family=fontfamily,face="bold"),
                legend.title = element_text(size = styling$legend$title$font$size,colour = styling$legend$title$font$color,margin=margin(0,0,0,0),family=styling$legend$title$font$family,face=styling$legend$title$font$face),
                
                legend.position = 'right',
                legend.text = element_text(family=styling$legend$labels$font$family,size = 20,colour = styling$legend$labels$font$color),
                #legend.margin = unit(1, "cm"),
                   legend.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),
                   plot.background = element_rect(fill = coc_styling$plot$color, colour = styling$plot$color))


  p <- p + theme(plot.margin = unit(c(70 + extramargins[1], 20 + extramargins[2], 50 + extramargins[3], 20 + extramargins[4]),"points"),
                 axis.title.y = element_text(size = styling$axis$labels$font$size, angle = 90,margin=margin(0,20,0,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
                 axis.title.x = element_text(size = styling$axis$labels$font$size,margin=margin(20,0,10,0),family=styling$axis$labels$font$family, colour = styling$axis$labels$font$color)
  )
if(class(data[[xvar]]) == "Date"){
    if(is.vector(xbreaks)){
      if(is.vector(xlabels)){
        p <- p + scale_x_date(limits = xlimits,breaks = xbreaks, labels = xlabels, expand = c(0,0))
      }else{
        p <- p + scale_x_date(limits = xlimits,breaks = xbreaks, expand = c(0,0))
      }
    }
  }else {
    if(is.vector(xbreaks)){
      if(is.vector(xlabels)){
        p <- p + scale_x_continuous(limits = xlimits,breaks = xbreaks, labels = xlabels, expand = c(0,0))
      }else{
        p <- p + scale_x_continuous(limits = xlimits,breaks = xbreaks, expand = c(0,0))
      }
    }
  }

  if(is.vector(ybreaks)){
    if(is.vector(ylabels)){
      p <- p + scale_y_continuous(limits = ylimits,breaks = ybreaks, labels = ylabels, expand = c(0,0))
    }else{
      p <- p + scale_y_continuous(limits = ylimits,breaks = ybreaks, expand = c(0,0))
    }
  }
  
  p <- p + xlab(xtitle)
  p <- p + ylab(ytitle)
  p <- p + labs(colour = legendtitle)
  #p <- p + scale_colour_manual(values = colpal)
  #Save to PNG
  #Save to PNG
  if(substr(path,nchar(path)-2,nchar(path))=="png"){
    savePNG(plot = p, path = path, showfooter=showfooter,width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, styling = styling)
  }else if(substr(path,nchar(path)-2,nchar(path))=="pdf"){
    savePDF(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, styling = styling)
  }
}