#Plotting
basicScatter <- function(data, xvar, yvar, colourvar = NULL, stat = "identity", pos = "dodge", flip = TRUE, title = "", subtitle = "", legendtitle = "", cite = "", author = "", path = NULL,  xtitle = "", xlimits = NULL, xbreaks = NULL, xlabels = NULL, ytitle = NULL, ylimits = NULL, ybreaks = NULL, ylabels = NULL, colpal = c("#B3B0D8", "#E4E499", "#F8ADAD", "#B2D9A2","#A0DADD", "#EFC786", "#919191", "#E2B9D7","#A05F6B","000000"), width = 800, height = 500, pointsize = 4, plotbackground = '#E8E8E8',  headerbackground = "#2E2E2E", headerfontcol = "white", footerbackground = "#2E2E2E", footerfontcol = "white", fontfamily = "Arial") {

  if(is.character(colourvar)){
    p <- ggplot(data, aes_string(x = xvar, y = yvar, colour = colourvar, fill = NULL))

   # p <- p + scale_colour_manual(values = colpal)
  }else{
    p <- ggplot(data, aes_string(x = xvar, y = yvar))
  }

  p <- p + theme(panel.grid.major.x = element_line(colour = "#AAAAAA"),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(colour = "#AAAAAA"),
                 plot.margin = unit(c(7, 2.5, 4, 2), "lines"),
                 axis.text = element_text(face = "bold", size = rel(1.3)),
                 axis.ticks = element_line(colour = NULL),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.line = element_line(colour = "black", size = 1.5),
                 #axis.line.y = element_blank(),
                 axis.title.y = element_text(size = rel(1.8), angle = 90,margin=margin(0,20,0,0),family=fontfamily,face="bold"),
                 axis.title.x = element_text(size = rel(1.8),margin=margin(20,0,0,0),family=fontfamily,face="bold"),
                 panel.background = element_rect(fill = plotbackground, colour = plotbackground),
                 legend.key.size = unit(.05,"npc"),
                 legend.key = element_rect(fill = plotbackground, colour = plotbackground),
                   legend.title = element_text(size = rel(1.8),margin=margin(0,20,0,50),family=fontfamily,face="bold"),
                   legend.position = 'right',
                   legend.text = element_text(face = "bold",size = 14),
                   #legend.margin = unit(1, "cm"),
                   legend.background = element_rect(fill = plotbackground, colour = plotbackground),
                   plot.background = element_rect(fill = plotbackground, colour = plotbackground)) 

  
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
  p <- p + scale_colour_manual(values = colpal)
  if(is.character(pointsize)){
    p <- p + scale_size("")
    p <- p + geom_point(aes_string(size = pointsize),alpha = .7)
  }else{
    p <- p + geom_point(size = pointsize)
  }

  #Save to PNG
  savePNG(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, headerbackground = headerbackground, headerfontcol = headerfontcol, footerbackground = footerbackground, footerfontcol = footerfontcol, fontfamily = fontfamily) 
}