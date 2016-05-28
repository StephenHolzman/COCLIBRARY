savePNG <- function(plot = NULL, path = getwd(), width = 600, height = 600, title = "", subtitle = "", cite = "", author = "", headerbackground = default_headerbackground, headerfontcol = default_headerfontcol, footerbackground = default_footerbackground, footerfontcol = default_footerfontcol, titlefont = default_titlefont,logo = default_logo){
  png(path, width = width, height = height)
  print(plot)
  grid.rect(x=unit(.3,"npc"),y=unit(0,"npc"),width=unit(2,"npc"),height=unit(75/height,"npc"),gp=gpar(fill=footerbackground, col = footerbackground))
  grid.rect(x=unit(.3,"npc"),y=unit(1,"npc"),width=unit(2,"npc"),height=unit(150/height,"npc"),gp=gpar(fill=headerbackground, col = headerbackground))
  grid.text(title, x=unit(0.01,"npc"),y=unit((height-26)/height,"npc"),just="left",gp=gpar(fontsize=36,fontfamily=fontfamily,fontface="bold", col=headerfontcol))
  grid.text(subtitle, x=unit(0.01,"npc"),y=unit((height-56)/height,"npc"),just="left",gp=gpar(fontsize=18,fontfamily=fontfamily,fontface="bold", col=headerfontcol))
  grid.text(cite, x=unit(0.01,"npc"),y=unit(20/height,"npc"),just="left",gp=gpar(fontsize=18,fontfamily=fontfamily,fontface="bold", col=footerfontcol))
  grid.text(author, x=unit(width-40,"points"),y=unit(20/height,"npc"),just="right",gp=gpar(fontsize=18,fontfamily=fontfamily,fontface="bold", col=footerfontcol))
  img <- readPNG(logo)
  g <- rasterGrob(img,x=unit(width-17,"points"),y=unit(20/height,"npc"),height=unit(30,"points"),width=unit(30,"points"))
  print(grid.draw(g), newpage = FALSE)
  dev.off() 
}