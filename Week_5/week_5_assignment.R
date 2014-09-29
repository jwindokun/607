# Adejare Windokun

# Week 5 Assignment

# 3 Question to ask.

#*******1***********

# Question 1. What is the total number of yes and no votes in Edinburgh and Glasgow?
# Question 2. What is the total number of votes in each city?
# Question 3. Number of votes per age group.

#*******2***********
#create a dataframe to store the information:
n = c("outcome", "Edinburgh.16-24", "Edinburgh.25+", "Glasgow.16-24", "Glasgow.25+")
x = c("Yes", 80100, 143000, 99400, 150400)
y = c("No", 35900, 214800, 43000, 207000)
polls = data.frame(rbind(x, y))
names(polls) = n
polls

#*******3***********
# Tidy the data using the tidyr package

#install.packages ("tidyr")
require (tidyr)
require (dplyr)
tidier <- polls %>% gather(city, votes, -outcome)
tidier

tidy <- tidier %>% separate(city, into = c("city", "age"), sep = "\\.")
tidy

#*******4***********
# User plyr package to answer the questions you asked

require (plyr)
tidy[,"votes"] = as.numeric(tidy[,"votes"])

c.votes = ddply(tidy, .(city, outcome), summarize, sumvotes = sum(votes))
# Question 1. What is the total number of yes and no votes in Edinburgh and Glasgow?

c.votes

# Question 2. What is the total number of votes in each city?
t.votes = ddply(c.votes, .(outcome), summarize, yvotes = sum(sumvotes))
t.votes

# Question 3. Number of votes per age group.
ddply(tidy, .(age), summarize, no.votes.by.age = sum(votes))
# Question 3. Number of votes per age group and outcome
ddply(tidy, .(age, outcome), summarize, no.votes.by.age.and.outcome = sum(votes))


#*******5***********
#Having gone through the process, would you ask different questions and/or change the way that you structured your data frame?

# No, not really, the initial data frame was messy, and that is what made in difficult to get out any answers. After it had
# been tidied, it was very easy to ask and obtain results using the plyr package.
# The initial data should have been stored the way the tidy data was finally stored after the use of the tidy package.
#
# With the tidy data, it is now possible to ask and easily carry out further statistical analysis - for example is
# there a difference in the proportion of yes votes in the different cities.
