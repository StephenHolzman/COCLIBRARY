savePNG <- function(plot = NULL, path = getwd(), width = 600, height = 600, title = "", subtitle = "", cite = "", author = "", headerbackground = "#2E2E2E", headerfontcol = "white", footerbackground = "#2E2E2E", footerfontcol = "white"){
  png(path, width = width, height = height)
  print(plot)
  grid.rect(x=unit(.3,"npc"),y=unit(0,"npc"),width=unit(2,"npc"),height=unit(75/height,"npc"),gp=gpar(fill=footerbackground, col = footerbackground))
  grid.rect(x=unit(.3,"npc"),y=unit(1,"npc"),width=unit(2,"npc"),height=unit(150/height,"npc"),gp=gpar(fill=headerbackground, col = headerbackground))
  grid.text(title, x=unit(0.01,"npc"),y=unit((height-26)/height,"npc"),just="left",gp=gpar(fontsize=36,fontfamily="Arial",fontface="bold", col=headerfontcol))
  grid.text(subtitle, x=unit(0.01,"npc"),y=unit((height-56)/height,"npc"),just="left",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col=headerfontcol))
  grid.text(cite, x=unit(0.01,"npc"),y=unit(20/height,"npc"),just="left",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col=footerfontcol))
  grid.text(author, x=unit(.99,"npc"),y=unit(20/height,"npc"),just="right",gp=gpar(fontsize=18,fontfamily="Arial",fontface="bold", col=footerfontcol))
  dev.off() 
}