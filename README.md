# UKParliamentData
Playing around with the data from the UK parliament API.

# API.R
This code accesses the parliment API and writes it into an SQLite DB

# Data Structure 
The API.R code will create an SQLite database so the user can access/ query the data rather than continue to use the API or cvs's.

Tables:
Members - This holds details on the MPs 
  - MP ID
  - Constituency ID
  - Full Name
  - Gender
  - Given Name
  - Party

ElectionResults - Contains results of elections by constituency
  - Result ID
  - Constituency ID
  - Election ID
  - Election Name
  - Electorate
  - Turn out
  - Marjority
  - Outcome

Constituency - Links constituency ID with name
  - Constituency ID
  - Constituency Name

Votes - Details on commons votes
  - Vote ID
  - Title
  - Date

MPVotes - How MPs voted (if they voted)
  - MP ID
  - Vote ID
  - Vote
