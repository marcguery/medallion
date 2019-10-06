setwd(Sys.getenv("PWD"))

library(reshape2)
library(stringr)
library(ggplot2)
library(plotly)
library(ade4)
library(factoextra)

options(stringsAsFactors = FALSE)
#################MEDALS########################
medals <- as.matrix(read.csv("medals.csv", header=F))
medals <- apply(medals, MARGIN = 2,
                FUN = function(x) {
                  subx <- sub(pattern = "^\'", replacement = "", trimws(x))
                  subx <- sub(pattern = "\'$", replacement = "", trimws(subx))
                  subx <- as.character(subx)
                  return(subx)
                  
                })
colnames(medals) <- paste(medals[1,], medals[2,])
medals <- as.data.frame(medals[-c(1,2,221),], row.names = medals[-c(1,2,221),1])
write.csv(medals, file = "medals-improved.csv")
medals <- read.csv("medals-improved.csv", row.names = 1)

medasum <- medals[,c(158:166)]
medasplit <- medals[,c(1:157)]

medasplit <- melt(data = medasplit, id.vars = c(1,2,3,4), value.name = "Medals")
medasplit <- cbind(medasplit[,-5], str_split_fixed(medasplit$variable, "\\.", n = 3))
colnames(medasplit) <- c("Country", "Code", "Lattitude", "Area", "Medals", "Date", "Rank", "Season")

medasplit$Date <- sub(pattern = "^X", replacement = "", medasplit$Date)
medasplit$Date <- sub(pattern = "\\D$", replacement = "", medasplit$Date)
medasplit$Season[medasplit$Season=="w"] <- "Winter"
medasplit$Season[medasplit$Season!="Winter"] <- "Summer"

countries <- read.csv("olympic-cities.csv")
colnames(countries)[3] <- "HostCountry"
medasplit <- merge(medasplit, countries, by = c("Date", "Season"))
#################FILTERING##################
medasum$all <- medasum$Gold.all + medasum$Silver.all + medasum$Bronze.all
quantile(medasum$all)
medasplit <- medasplit[medasplit$Country%in%(row.names(medasum[medasum$all>30.75,])),]
###########################################
##################################################
######################RAPID OVERVIEW###################"
medaview <- medasplit[medasplit$Country%in%row.names(medasum)[medasum$Gold.all>400],]

gg <- ggplot(medaview, aes(y=Medals, x=Date)) + 
  geom_crossbar(aes(ymin=0, ymax=Medals, col=Country)) +
  labs(subtitle="Country vs medals", 
       y="Medals", 
       x="Country", 
       caption = "US vs Soviet Union")

plot(gg)

gg <- ggplot(subset(medasplit, Country=='South Korea' & Season=='Summer'), aes(y=Medals, x=Date)) + 
  geom_bar(stat = "identity", na.rm=TRUE, aes(color=HostCountry, fill=Rank), size=2) +
  labs(subtitle="Medals per year", 
       y="Medals", 
       x="Year", 
       caption = "Sout Korea")

plot(gg)
ok <- plotly_build(gg)
ok
##################################################
#######################SUMMER#####################
summermedals <- subset(medasplit, Season=="Summer")

gg <- ggplot(summermedals, aes(y=Medals, x=HostCountry)) + 
  geom_jitter(aes(color=Country, shape=Rank), size=2) +
  labs(subtitle="Medals per year", 
       y="Medals", 
       x="Year", 
       caption = "Sout Korea")

gg
ok <- plotly_build(gg)
ok

medasplit$Date <- as.numeric(medasplit$Date)
medasplit$Medals <- as.numeric(medasplit$Medals)



