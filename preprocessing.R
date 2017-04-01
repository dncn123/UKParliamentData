library(dplyr)
library(reshape2)
library(ggplot2)
library(RSQLite)

## Set directory
setwd("C:/Users/dstannett/Documents/Politics Data")

## Connect to DB
db <- dbConnect(SQLite(), dbname="politics.sqlite")

## This data is structured in a really awkward way 
## To get the full term MP for the 2010 term we must do the following

## Election results for 2010 parliament give the conID s
## Votes that took place in 2010 parliament give the voteIDs
## Can get the MPIDs that voted in this session
## and remove IDs have multiple per conID (had a bi-election)
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

mpvotes <- dbGetQuery(db, 
                      paste("select *
                            from mpvotes
                            where MPID in (", paste(members2010$MPID, collapse=","), ")
                            and voteID in (select voteID from votes where date between '2010-05-06' and '2015-05-06')", sep=""))

## Add numeric value to mpvote data
mpvotes$votenum <- NA
mpvotes[which(mpvotes$vote == "AyeVote"),]$votenum <- 1
mpvotes[which(mpvotes$vote == "NoVote"),]$votenum <- -1

head(mpvotes)

## Cast the data to get votes as columns 
vote_matrix <- dcast(mpvotes, MPID ~ voteID, value.var = "votenum", fun.aggregate = mean)
head(vote_matrix[,c(1:15)])

write.csv(vote_matrix, 
          file="vote_matrix.csv", 
          row.names = FALSE)

