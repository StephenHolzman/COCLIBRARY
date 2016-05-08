#Load libraries
library(blsAPI)
library(rjson)
library(dplyr)
library(ggplot2)
library(grid)
library(reshape2)
#Build request parameters for the BLS API
parameters <- list(
  "seriesid" = c("LNS14000004", "LNS14000005","LNS14000007","LNS14000008"),
  "startyear" = 2007,
  "endyear" = 2016
)

#Send the request, store the response
response <- blsAPI(parameters,2)

#Convert response json to R list
json <- fromJSON(response)

#Establish dimensions of matrix to store rates
totalMonths <- length(json$Results$series[[1]]$data)
totalSeries <- length(parameters$seriesid)

#Set up dataframe for graphing
df <- matrix(nrow=totalMonths,ncol=totalSeries+2)

#Go through results and assign rates/dates to proper matrix coordinate
seriesCount <- 1
for(series in json$Results$series){
  dateCount <- 1
  for(item in json$Results$series[[seriesCount]]$data){
    df[dateCount,5] <- as.Date(paste(item$year,"-",substr(item$period,2,3),"-","01",sep=""))
    df[dateCount,6] <- paste(item$year,"-",substr(item$period,2,3),"-","01",sep="")
    
    df[dateCount,seriesCount] <- as.numeric(item$value)
    dateCount <- dateCount + 1
  }
  seriesCount <- seriesCount + 1
}
#Name columns
colnames(df) <- c("White_Male","White_Female","Black_Male","Black_Female","Date","Date_String")

#Convert to data frame
df <- data.frame(df)

#Sort by date
df <- arrange(df, Date)

#Make sure data types are numeric or dates
df$White_Male <- as.numeric(as.character(df$White_Male))
df$White_Female <- as.numeric(as.character(df$White_Female))
df$Black_Male <- as.numeric(as.character(df$Black_Male))
df$Black_Female <- as.numeric(as.character(df$Black_Female))

df$Date <- as.numeric(as.character(df$Date))
df$Date_String <- as.Date(as.character(df$Date_String))

df$Date <- NULL
melted <- melt(df, id.vars=c("Date_String"))
melted$variable <- gsub("_"," ",melted$variable)
#Plotting
basicMultiline <- function(data, xvar, title, subtitle, cite, author, ylabel, xlabel, path, ylimits, ybreaks, ylabels) {

  p <- ggplot(data, aes_string(x = xvar, y = "value", colour = "variable"))
  p <- p + theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(colour = "#AAAAAA"),
                 plot.margin = unit(c(7, 2.5, 3, 2), "lines"),
                 axis.text = element_text(face = "bold", size = rel(1.3)),
                 axis.ticks = element_line(colour = NULL),
                 axis.ticks.y = element_blank(),
                 axis.ticks.x = element_line(colour = "black", size = 2),
                 axis.line = element_line(colour = "black", size = 1.5),
                 axis.line.y = element_blank(),
                 axis.title.y = element_text(size = rel(1.8), angle = 90,margin=margin(0,20,0,0),family="Arial",face="bold"),
                 panel.background = element_rect(fill = 'white'),
                 legend.key.size = unit(.05,"npc"),
                 legend.key = element_rect(fill = 'white'),
                 legend.title = element_blank(),
                 legend.text = element_text(face = "bold",size = 14))
  p <- p + geom_line(size=2)

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
basicMultiline(data = melted,
               xvar = "Date_String",
               title = "USA National Unemployment Rates",
               subtitle = "Seasonally Adjusted, By Race & Sex, Age 16+",
               cite = "Source: Bureau of Labor Statistics",
               author = "@StephenHolz",
               ylabel = "Unemployment Rate",
               xlabel = "",
               ylimits = c(0,22),
               ybreaks = c(0, 5, 10, 15, 20),
               ylabels = c("0%","5%","10%","15%","20%"),
               path = "/Volumes/Storage/UnemploymentByRace.png"
)
testdf <- NULL
testdf$id <- c(1,2,3,4,5)
testdf$random1 <-c(10,20,30,24,12)
testdf$random2 <-c(33,45,27,18,10)
testdf <- data.frame(testdf)

testmelt <- melt(testdf, id.vars=c("id"))
basicMultiline(data = testmelt,
               xvar = "id",
               title = "Random Numbers",
               subtitle = "Completely Made Up",
               cite = "Source: My Ass",
               author = "@StephenHolz",
               ylabel = "Y Scale",
               xlabel = "",
               ylimits = c(0,50),
               ybreaks = c(0, 10, 20, 30, 40, 50),
               ylabels = c("$0","$10","$20","$30","$40","$50"),
               path = "/Volumes/Storage/testnumeric.png"
)
