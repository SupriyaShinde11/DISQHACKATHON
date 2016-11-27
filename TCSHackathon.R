setwd("D:/RdataWork")                
getwd() 
data<- read.csv(file="matches.csv",head=TRUE,sep=",") 
data
data1<- read.csv(file="Deliveries.csv",head=TRUE,sep=",")
data1

install.packages("ggplot2")
install.packages("tm")
install.packages("sentiment.tar.gz", repos=NULL, type="source")
library("sentiment")
library("Rstem")
library("NLP")
library("slam")
library("tm")
library("Rstem", lib.loc="~/R/win-library/3.2")
library("ggplot2")
library("proto")
library("plyr")
library("RColorBrewer")
data= gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data)
data= gsub("@\\w+", "", data)
data = gsub("[[:punct:]]", "",data)
data = gsub("[[:digit:]]", "", data)
data= gsub("http\\w+", "", data)
data = gsub("[ \t]{2,}", "", data)
data = gsub("^\\s+|\\s+$", "", data)

data= sapply(data, catch.error)
postcatch.error = function(x)
{
  +              # let us create a missing value for test purpose
    +                  y = NA
    +                  # try to catch that error (NA) we just created
      +                  catch_error = tryCatch(tolower(x), error=function(e) e)
      +                      # if not an error
        +                          if (!inherits(catch_error, "error"))
          +                                  y = tolower(x)
          +                              # check result if error exists, otherwise the function works fine.
            +                                  return(y)
}


data= data[!is.na(data)]
data


(wwt <- hist(women$weight,nclass = 7, plot = FALSE))
plot(wwt, labels = TRUE) # default main & xlab using wwt$xname
plot(wwt, border = "dark blue", col = "light blue",
     main = "Histogram of the Performance of the Cricket", xlab = "")

library(wordcloud)
wordcloud(data)

if (!require("cricketr")){ 
  install.packages("cricketr",lib = "c:/test") 
} 
library(cricketr)



data1 <- getPlayerData(35320,dir="D:/RdataWork",file="deliveries.csv",type="batting",homeOrAway=c(1,2),
                           result=c(1,2,4))

batsmanAvgRunsGround("./deliveries.csv","SC Ganguly")





library(plotly)

p <- plot_ly(
  x = c("season", "city", "winner"),
  y = c(20, 14, 23),
  name = "Performance",
  type = "bar"
)


library(sqldf)
sqldf("select batsman count(*) from data1 where extra_runs is not null group by total_runs")
library(ggplot2)

DF=sqldf("select batsman from data1 where extra_runs=0")
qplot(DF$total_runs,data1=DF, geom='histogram')


sqldf("select match_id,inning,batsman count(*) from data1 where batsman_runs is not null group by extra_runs")



# Create the data for the chart.
H <- c(7,12,28,3,41)
M <- c(2008,2009,2010,2011,2012,2013,2014,2015)

# Give the chart file a name.
png(file = "barchart_year_revenue.png")

# Plot the bar chart.
barplot(H,names.arg = M,xlab = "year",ylab = "Revenue",col = "blue",
        main = "Revenue chart",border = "red")

# Save the file.
dev.off()