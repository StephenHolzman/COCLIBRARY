#Plotting
basicScatter <- function(data, xvar, yvar, colourvar, title, subtitle, cite, author, ylabel, xlabel, path, ylimits, ybreaks, ylabels) {

  p <- ggplot(data, aes_string(x = xvar, y = yvar, colour = colourvar))
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
                 axis.title.y = element_text(size = rel(1.8), angle = 90,margin=margin(0,20,0,0),family="Arial",face="bold"),
                 axis.title.x = element_text(size = rel(1.8),margin=margin(20,0,0,0),family="Arial",face="bold"),
                 panel.background = element_rect(fill = 'white'),
                 legend.key.size = unit(.05,"npc"),
                 legend.key = element_rect(fill = 'white'),
                 legend.title = element_blank(),
                 legend.text = element_text(face = "bold",size = 14))
  p <- p + geom_point(size=4)

  if(class(data[[xvar]]) == "Date"){
    p <- p + scale_x_date()
  }else {
    p <- p + scale_x_continuous()
  }

  p <- p + scale_y_continuous(limits = ylimits,breaks = ybreaks, labels = ylabels, expand = c(0,0))
  p <- p + xlab(xlabel)
  p <- p + ylab(ylabel)
  p <- p + scale_colour_manual(values = c("#B3B0D8", "#E4E499", "#F8ADAD", "#A0DADD", "#EFC786", "#919191", "#E2B9D7", "#B2D9A2","#A05F6B","000000"))

  #Save to PNG
  png(path, width = 800, height = 500)
  print(p)
  grid.rect(x=unit(.3,"npc"),y=unit(0,"npc"),width=unit(2,"npc"),height=unit(.15,"npc"),gp=gpar(fill="#2E2E2E"))
  grid.rect(x=unit(.3,"npc"),y=unit(1,"npc"),width=unit(2,"npc"),height=unit(.35,"npc"),gp=gpar(fill="#2E2E2E"))
  grid.text(title, x=unit(0.01,"npc"),y=unit(.95,"npc"),just="left",gp=gpar(fontsize=36,fontfamily="Arial",fontface="bold", col="white"))
  grid.text(subtitle, x=unit(0.01,"npc"),y=unit(.88,"npc"),just="left",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col="white"))
  grid.text(cite, x=unit(0.01,"npc"),y=unit(.04,"npc"),just="left",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col="white"))
  grid.text(author, x=unit(.99,"npc"),y=unit(.04,"npc"),just="right",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col="white"))
  dev.off()  
}