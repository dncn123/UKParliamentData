## Load Packages
install.packages("httr")
install.packages("JSONLITE")
install.packages("RSQLite")
install.packages("sqldf")
library(httr)
library(jsonlite)
library(RSQLite)
library(sqldf)

setwd("C:/Users/dstannett/Documents/Politics Data")

## Get Data on Members of HoC, Electrion Results and Voting Records
webcontent <- function(URL){
  result <- GET(URL)
  if(http_status(result)$category=="Success"){
    return(fromJSON(toJSON(content(result))))
  }
  else{
    print("API ERROR")
  }
}

## Members API
membersresult <- webcontent("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=100000")

## Create Members Dataframe
MPID = as.numeric(substr(membersresult$result$items$`_about`,
                         max(gregexpr("/", membersresult$result$items$`_about`)[[1]])+1,
                         nchar(membersresult$result$items$`_about`)))
conID = as.numeric(substr(membersresult$result$items$constituency$`_about`,
                          max(gregexpr("/", membersresult$result$items$constituency$`_about`)[[1]])+1,
                          nchar(membersresult$result$items$constituency$`_about`)))
fullName = as.character(unlist(membersresult$result$items$fullName$`_value`))
gender = as.factor(unlist(membersresult$result$items$gender$`_value`))
givenName = as.character(unlist(membersresult$result$items$givenName$`_value`))
party = unlist(membersresult$result$items$party$`_value`)
members <- data.frame(MPID, conID, fullName, gender, givenName, party)
members$fullName <- as.character(members$fullName)
members$givenName <- as.character(members$givenName)

## Members Summary
dim(members)
str(members)
summary(members)

## This pulls all the members of parliament which will be subset later

## Election Results API
electionresults <- webcontent("http://lda.data.parliament.uk/electionresults.json?_pageSize=100000")

## Create Election Results Dataframe
resultsID = as.numeric(substr(electionresults$result$items$`_about`,
                              max(gregexpr("/", electionresults$result$items$`_about`)[[1]])+1,
                              nchar(electionresults$result$items$`_about`)))

conID = as.numeric(substr(electionresults$result$items$constituency$`_about`,
                          max(gregexpr("/", electionresults$result$items$constituency$`_about`)[[1]])+1,
                          nchar(electionresults$result$items$constituency$`_about`)))

electionID = as.numeric(substr(electionresults$result$items$election$`_about`,
                               max(gregexpr("/", electionresults$result$items$election$`_about`)[[1]])+1,
                               nchar(electionresults$result$items$election$`_about`)))

electionNAME = unlist(electionresults$result$items$election$label$`_value`)
electorate = unlist(electionresults$result$items$electorate)
turnout = unlist(electionresults$result$items$turnout)
marjority = unlist(electionresults$result$items$majority)
outcome = unlist(electionresults$result$items$electorate)
electionresults <- data.frame(resultsID,conID,electionID,
                              electionNAME,electorate,
                              turnout,marjority,outcome)

## Election Results Summary
dim(electionresults)
str(electionresults)
summary(electionresults)

## Constituency API and DataFrame
constituency <- data.frame()
for(i in members$conID){
  URLQUERY <- paste("http://lda.data.parliament.uk/constituencies/", i, ".json", sep = "")
  con <- webcontent(URLQUERY)
  constituency <- rbind(constituency,
                        data.frame(conID = as.numeric(substr(con$result$primaryTopic$`_about`,
                                                             max(gregexpr("/", con$result$primaryTopic$`_about`)[[1]])+1,
                                                             nchar(con$result$primaryTopic$`_about`))), 
                                   name = gsub(",","",con$result$primaryTopic$label$`_value`)))
}

## Constituency API Summary
head(constituency)
dim(constituency)
summary(constituency)

## Votes API and DataFrame - used to get unique ID of each vote that can then be looped over
votes <- webcontent("http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=100000")
voteID = as.numeric(substr(votes$result$items$`_about`,
                           max(gregexpr("/", votes$result$items$`_about`)[[1]])+1,
                           nchar(votes$result$items$`_about`)))
title <- unlist(votes$result$items$title)
date <- unlist(votes$result$items$date$`_value`)
votes <- data.frame(voteID, title, date)

## Votes Summary
head(votes)
dim(votes)
summary(votes)

## API and DataFrame for how MPs voted (This is really really slow)
voteID_to_do <- votes$voteID
mpvotes <- data.frame()

start_time <- Sys.time()
while(length(voteID_to_do) > 0 & as.numeric(difftime(Sys.time(), start_time, units="secs")) < 3600){
  
  for(i in voteID_to_do){
    
    mpvotescontent <- NULL
    mpvotescontent <- try(webcontent(paste("http://lda.data.parliament.uk/commonsdivisions/id/", i, ".json", sep = "")))
    if(!is.null(mpvotescontent)){
      voteID <- rep(i, length(mpvotescontent$result$primaryTopic$vote$member))
      MPID <- c()
      
      for(j in 1:length(mpvotescontent$result$primaryTopic$vote$member)){
        MPID[j] = as.numeric(substr(mpvotescontent$result$primaryTopic$vote$member[[j]]$`_about`,
                                    max(gregexpr("/", mpvotescontent$result$primaryTopic$vote$member[[j]]$`_about`)[[1]])+1,
                                    nchar(mpvotescontent$result$primaryTopic$vote$member[[j]]$`_about`)))
      }
      
      vote = substr(mpvotescontent$result$primaryTopic$vote$type,
                    max(gregexpr("#", mpvotescontent$result$primaryTopic$vote$type)[[1]])+1,
                    nchar(mpvotescontent$result$primaryTopic$vote$type))
      mpvotes <- rbind(mpvotes,data.frame(voteID,MPID,vote))
    }
  }
  voteID_to_do <- voteID_to_do[!(voteID_to_do %in% unique(mpvotes$voteID))]
}

## MP Votes Summary
head(mpvotes)
dim(mpvotes)
summary(mpvotes)

## Create SQL lite database in R to make access to data from API easy
db <- dbConnect(SQLite(), dbname="politics.sqlite")

write.csv(members, file = "members.csv", row.names = FALSE)
write.csv(electionresults, file = "electionresults.csv", row.names = FALSE)
write.csv(constituency, file = "constituency.csv", row.names = FALSE)
write.csv(votes, file = "votes.csv", row.names = FALSE)
write.csv(mpvotes, file = "mpvotes.csv", row.names = FALSE)

dbWriteTable(conn=db, name="members", value=members,
             row.names=FALSE, overwrite=TRUE)
dbWriteTable(conn=db, name="electionresults", value=electionresults,
             row.names=FALSE, overwrite=TRUE)
dbWriteTable(conn=db, name="constituency", value=constituency,
             row.names=FALSE, overwrite=TRUE)
dbWriteTable(conn=db, name="votes", value=votes,
             row.names=FALSE, overwrite=TRUE)
dbWriteTable(conn=db, name="mpvotes", value=mpvotes,
             row.names=FALSE, overwrite=TRUE)

dbListTables(db)
