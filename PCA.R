install.packages("dplyr")
install.packages("reshape2")
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("C:/Users/dstannett/Documents/Politics Data")

vote_matrix <- read.csv("vote_matrix.csv")
head(vote_matrix[,c(1:15)])

vote_matrix[is.na(vote_matrix)] <- 0

votespca <- data.frame(MPID = vote_matrix$MPID,
                       prcomp(vote_matrix[,-1])$x)
## This data wasn't centred or scaled before going through PCA
## the votes with larger variances will be more differentiating so 
## I want them to contribute more

members2010 <- dbGetQuery(db, 
                          "select *
                          from members 
                          where conID in (select conID from electionresults where electionID = 382037)
                          and MPID in (select distinct MPID 
                          from mpvotes
                          where voteID in (select voteID from votes where date between '2010-05-06' and '2015-05-06'))
                          group by conID
                          having count(distinct MPID) = 1")

head(members2010)

votespca <- merge(x = votespca, y = members2010[,c("MPID","party")], by = "MPID")

head(votespca)
str(votespca[c(1,610,611)])

## Add colours for parties
distinct(votespca, party)
values = c("Labour"="red",
           "Labour (Co-op)"="red",
           "Conservative"="blue",
           "Liberal Democrat"="orange",
           "Scottish National Party"="yellow",
           "Speaker"="grey",
           "Independent"="grey",
           "Plaid Cymru"="grey",
           "Democratic Unionist Party"="grey",
           "Sinn Fein"="grey",
           "UK Independence Party"="purple",
           "Social Democratic & Labour Party"="grey",
           "Alliance"="grey",
           "Green Party"="green")

votespca$party <- as.factor(votespca$party)
votespca$col <- as.factor(votespca$col)

g <- ggplot(votespca) + geom_point(aes(x=PC1, y=PC2, col=votespca$party))
g + scale_colour_manual(values=values)

## hummm really did come out as left and right....
