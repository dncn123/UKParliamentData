library(RSQLite)

setwd("C:/Users/dstannett/Documents/Politics Data")

vote_matrix <- read.csv("vote_matrix.csv")
head(vote_matrix[,c(1:15)])

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

vote_matrix <- merge(x = vote_matrix, y = members2010[,c("MPID","party")], by = "MPID")

head(vote_matrix[,c(1:12)])

## Covariance similarity
row.names(vote_matrix) <- vote_matrix$MPID
inver_vm <- data.frame(t(vote_matrix[,c(2:1239)]))

getcosin <- function(data, id1, id2){
  x <- inver_vm[paste("X",as.character(id1),sep="")]
  y <- inver_vm[paste("X",as.character(id2),sep="")]
  subset <- (!is.na(x) & !is.na(y))
  x <- x[subset]
  y <- y[subset]
  return(as.numeric(x %*% y / sqrt(x%*%x * y%*%y)))
}

getcosin(inver_vm,2,8)

MPsimil <- matrix(nrow=length(vote_matrix$MPID), 
                  ncol=length(vote_matrix$MPID))
rownames(MPsimil) <- vote_matrix$MPID
colnames(MPsimil) <- vote_matrix$MPID

## Slightly slowly but can't do it in one as removing to NAs
for(i in 1:length(vote_matrix$MPID)){
  for(j in 1:length(vote_matrix$MPID)){
    MPsimil[i,j] <- getcosin(inver_vm,
                             vote_matrix$MPID[i],
                             vote_matrix$MPID[j])
    if(i==j){print(i)}
  }
}

## So data actually looks really boring
## It seems they are just vote on party lines 
hist(MPsimil)


## To be continued.... with some graph analysis

