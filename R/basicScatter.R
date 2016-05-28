#Plotting
basicScatter <- function(data, xvar, yvar, colourvar = NULL, stat = "identity", pos = "dodge", flip = TRUE, title = "", subtitle = "", legendtitle = "", cite = "", author = "", path = NULL,  xtitle = "", xlimits = NULL, xbreaks = NULL, xlabels = NULL, ytitle = NULL, ylimits = NULL, ybreaks = NULL, ylabels = NULL, colpal = default_colpal, width = 800, height = 500, pointsize = 4, plotbackground = default_plotbackground,  headerbackground = default_headerbackground, headerfontcol = default_headerfontcol, footerbackground = default_footerbackground, footerfontcol = default_footerfontcol, titlefont = default_titlefont, labelfont = default_labelfont, labelfontcol = default_labelfontcol, logo = default_logo) {

  if(is.character(colourvar)){
    p <- ggplot(data, aes_string(x = xvar, y = yvar, colour = colourvar, fill = NULL))
    if(is.character(pointsize)){
      p <- p + scale_size("")
      p <- p + geom_point(aes_string(size = pointsize),alpha = .7)
    }else{
      p <- p + geom_point(size = pointsize,alpha = .7)
    }
   # p <- p + scale_colour_manual(values = colpal)
  }else{
    p <- ggplot(data, aes_string(x = xvar, y = yvar))
    if(is.character(pointsize)){
      p <- p + scale_size("")
      p <- p + geom_point(aes_string(size = pointsize),alpha = .7)
    }else{
      p <- p + geom_point(size = pointsize,alpha = .7, colour = colpal[1])
    }
  }

  p <- p + theme(panel.grid.major.x = element_line(colour = "#AAAAAA"),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(colour = "#AAAAAA"),
                 plot.margin = unit(c(5, 2, 4, 1), "lines"),
                 axis.text = element_text(face = "bold", size = rel(1.3), family = labelfont),
                 axis.ticks = element_line(colour = NULL),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.line = element_line(colour = "black", size = 1.5),
                 #axis.line.y = element_blank(),
                 axis.title.y = element_text(size = rel(1.8), angle = 90,margin=margin(0,20,0,0),family=labelfont),
                 axis.title.x = element_text(size = rel(1.8),margin=margin(20,0,0,0),family=labelfont),
                 panel.background = element_rect(fill = plotbackground, colour = plotbackground),
                 legend.key.size = unit(.05,"npc"),
                 legend.key = element_rect(fill = plotbackground, colour = plotbackground),
                 legend.title = element_text(size = rel(1.8),margin=margin(0,20,0,50),family=titlefont,face="bold"),
                 legend.position = 'top',
                 legend.text = element_text(family=labelfont,size = 20,colour = labelfontcol),
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


  #Save to PNG
  savePNG(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, headerbackground = headerbackground, headerfontcol = headerfontcol, footerbackground = footerbackground, footerfontcol = footerfontcol, titlefont = titlefont, logo = logo) 
}