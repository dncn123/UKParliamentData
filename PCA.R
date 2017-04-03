install.packages("dplyr")
install.packages("reshape2")
install.packages("tsne")
library(tsne)
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

db <- dbConnect(SQLite(), dbname="politics.sqlite")

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

g <- ggplot(votespca) + geom_point(aes(x=PC1, y=PC2, col=votespca$party))
g + scale_colour_manual(values=values)

## hummm really did come out as left and right....
## Also it makes sense that Labour are negative as they were in opposition 
## So were voting against most of the budgets etc 

## Now to try tsne which is much better at finding local clustering
## So hopefully we can see the political fractions within parties...

## tSNE doesn't work well in really high dimensions so we feed it the 
## first 30 PCs

votestsne <- tsne(votespca[,-c(1,610)])
votestsne <- data.frame(votestsne)
votestsne$party <- votespca$party
names(votestsne) <- c("D1", "D2", "party")
g <- ggplot(votestsne) + geom_point(aes(x=D1, y=D2, col=votestsne$party))
g + scale_colour_manual(values=values)

## I love t-SNE...
## I guess the thing to watch out for it this visualisation makes labour/torys
## look much more simialr. You would be wrong the think that an MP of the egde of
## Their parties cluster in tsne is closer to the opposition than their party
## I should optimise tsne to give the best results base on perplexity

tsnetries <- list()
for(i in c(5,10,20,30,40,50,100)){
  tsnetries[[i]] <- tsne(votespca[,-c(1,610)], perplexity = i)
}

i = c(1,5,10,20,30,40,50,100)[5]
tsnetries[[i]] <- data.frame(tsnetries[[i]])
tsnetries[[i]]$party <- votespca$party
names(tsnetries[[i]]) <- c("D1", "D2", "party")
g <- ggplot(tsnetries[[i]]) + geom_point(aes(x=D1, y=D2, col=tsnetries[[i]]$party))
g + scale_colour_manual(values=values)


## It looks like 30 is the best