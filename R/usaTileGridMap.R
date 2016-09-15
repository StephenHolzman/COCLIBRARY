usaTileGridMap <- function(data,id,yvar,labelvar,legendlimits=NULL,legendbreaks=NULL,legendlabels=NULL,width=720,height=600,path=NULL,title="",subtitle="",legendtitle=NULL,cite="",author="",plotbackground=default_plotbackground,headerbackground=default_headerbackground,headerfontcol=default_headerfontcol,footerbackground=default_footerbackground,footerfontcol=default_footerfontcol,titlefont=default_titlefont,labelfont=default_labelfont,labelfontcol=default_labelfontcol,colpal = styling$colors$main,logo=default_logo,styling=coc_styling,extramargins=c(0,0,0,0)){

  mapCoordinates <- data.frame(
    postal = c('ME','AK','VT','NH','MA','WA','MT','ND','SD','MN','WI','MI','NY','CT','RI','OR','ID','WY','NE','IA','IL','IN','OH','PA','NJ','CA','NV','UT','CO','KS','MO','KY','WV','DC','MD','DE','AZ','NM','OK','AR','TN','VA','NC','HI','TX','LA','MS','AL','GA','SC','FL'),
    ap = c('Maine','Alaska','Vt.','N.H.','Mass.','Wash.','Mont.','N.D.','S.D.','Minn.','Wis','Mich.','N.Y.','Conn','R.I.','Ore.','Idaho','Wyo.','Neb.','Iowa','Ill.','Ind.','Ohio','Pa.','N.J.','Calif.','Nev.','Utah','Colo.','Kan.','Mo.','Ky.','W.Va.','D.C.','Md.','Del.','Ariz.','N.M.','Okla.','Ark.','Tenn.','Va.','N.C.','Hawaii','Texas','La.','Miss.','Ala.','Ga.','S.C.','Fla.'),
    name = c('Maine','Alaska','Vermont','New Hampshire','Massachusetts','Washington','Montana','North Dakota','South Dakota','Michigan','Wisconsin','Minnesota','New York','Connecticut','Rhode Island','Oregon','Idaho','Wyoming','Nebraska','Iowa','Illinois','Indiana','Ohio','Pennsylvania','New Jersey','California','Nevada','Utah','Colorado','Kansas','Missouri','Kentucky','West Virginia','District of Columbia','Maryland','Delaware','Arizona','New Mexico','Oklahoma','Arkansas','Tennessee','Virginia','North Carolina','Hawaii','Texas','Louisiana','Mississippi','Alabama','Georgia','South Carolina','Florida'),
    fips = c(23,2,50,33,25,53,30,38,46,27,55,26,36,9,44,41,16,56,32,19,17,18,39,42,34,6,32,49,8,20,29,21,54,11,24,10,4,35,40,5,47,51,37,15,48,22,28,1,13,45,12),
    X = c(12,1,10,11,12,1,2,3,4,5,6,8,10,11,11,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,10,9,10,3,4,5,6,7,9,8,1,4,5,5,6,7,8,7),
    Y = c(7,7,6,7,6,5,5,5,5,5,5,5,5,5,6,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,2,3,3,2,2,2,2,2,2,2,1,1,0,1,1,1,1,0),
    stringsAsFactors = FALSE
  ) 
  
  mapJoinData <- left_join(mapCoordinates,data,id)
  p <- ggplot(mapJoinData,aes_string(x = "X", y = "Y",label="ap"))
  p <- p + geom_tile(aes_string(fill = yvar),colour=coc_styling$plot$color,size=coc_styling$grid$lines$size/2)
  
  legendorientation <- "top"
  p <- p + theme(plot.margin = unit(c(70 + extramargins[1], 20 + extramargins[2], 50 + extramargins[3], 20 + extramargins[4]),"points"),
                 axis.title.y = element_blank(),
                 axis.title.x = element_blank()
  )
  p <- p + theme(
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = styling$margins$color, colour = styling$plot$color),
    legend.key.height = unit(25,"points"),
    legend.key.width = unit(25,"points"),
    legend.key = element_rect(fill = styling$legend$color, colour = styling$legend$color),
    legend.title = element_text(size = styling$legend$title$font$size,margin=margin(0,0,0,0),family=styling$legend$title$font$family,face=styling$legend$title$font$face),
    legend.position = c(.85,.1),
    legend.direction = "horizontal",
    legend.text = element_text(family=styling$legend$labels$font$family,size = 20,colour = styling$legend$labels$font$color),
    legend.background = element_rect(fill = styling$legend$color, colour = styling$legend$color),
    plot.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),
    strip.background = element_rect(fill = styling$facet$labels$color, colour = styling$facet$labels$color),
    strip.text = element_text(colour=styling$facet$labels$font$color,size=styling$facet$labels$font$size,family=styling$facet$labels$font$family)
  )
  #p <- p + xlab(xtitle)
  #p <- p + ylab(ytitle)
  #p <- p + facet_grid(reformulate("Age.of.Mother"),scales="free")
  #p <- p + scale_fill_manual(values = colpal)
  #p <- p + scale_fill_gradient2(low = colpal[2],mid = "white", high = colpal[1])
  p <- p + scale_fill_gradient(low="white",high = colpal[1],limits=legendlimits,breaks=legendbreaks,labels=legendlabels)
  #p <- p + scale_fill_brewer()
  p <- p + labs(fill = legendtitle)
  p <- p + geom_text(aes_string(label="ap"),vjust=-.2,colour=styling$legend$labels$font$color,size=7,family=styling$legend$labels$font$family)
  p <- p + geom_text(aes_string(label=labelvar),vjust=1.75,colour=styling$legend$labels$font$color,size=5,family=styling$legend$labels$font$family)
  p <- p + guides(colour = guide_legend(override.aes = list(alpha = .2)))
  savePNG(plot = p, path = path, width = width, height = height, title = title, subtitle = subtitle, cite = cite, author = author, styling = styling)
  
  
}
