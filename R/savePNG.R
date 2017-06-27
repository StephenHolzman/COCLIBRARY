savePNG <- function(plot = NULL, path = getwd(), logo = TRUE,width = 600, height = 600, title = "", subtitle = "", cite = "", author = "", styling = coc_styling,showfooter=TRUE){
  png(path, width = width, height = height)
  #print(plot)
  if(showfooter){
    grid.rect(x=unit(.3,"npc"),y=unit(0,"npc"),width=unit(2,"npc"),height=unit(styling$footer$height,"points"),gp=gpar(fill=styling$footer$color, col = styling$footer$color))
    grid.text(cite, x=unit(width-8,"points"),y=unit(12,"points"),just="right",gp=gpar(fontsize=styling$footer$cite$font$size,fontfamily=styling$footer$cite$font$family,fontface=styling$footer$cite$font$face, col=styling$footer$cite$font$color))
    grid.text(author, x=unit(width-8,"points"),y=unit(26,"points"),just="right",gp=gpar(fontsize=styling$footer$author$font$size,fontfamily=styling$footer$author$font$family,fontface=styling$footer$author$font$face, col=styling$footer$author$font$color))
    img <- styling$logos$icon$svg
    g <- rasterGrob(img,x=unit(185,"points"),y=unit(20/height,"npc"),height=unit(40,"points"))
    if(logo){print(grid.draw(g), newpage = FALSE)}    
  }
  grid.rect(x=unit(.3,"npc"),y=unit(1,"npc"),width=unit(2,"npc"),height=unit(styling$header$height,"points"),gp=gpar(fill=styling$header$color, col = styling$header$color))
  grid.text(title, x=unit(0.01,"npc"),y=unit((height-26)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$title$font$size,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
  grid.text(subtitle, x=unit(0.01,"npc"),y=unit((height-56)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$subtitle$font$size,fontfamily=styling$header$subtitle$font$family,fontface=styling$header$subtitle$font$face, col=styling$header$subtitle$font$color))

  dev.off() 
}