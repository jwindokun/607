#***************************Adejare Windokun*****************

movies <- read.delim("C:/Users/jare/SkyDrive/WorkDocs/CUNY/607/Week 4/movies.tab", header=TRUE, stringsAsFactors = FALSE)
#head(movies)
#str(movies)
# will take out columns that are not needed
#install.packages("ggplot2") graphic package, already installed
#movies = subset(movies, select = c(-r1, -r2, -r3, -r4, -r5, -r6, -r7, -r8, -r9, -r10))
#head(movies)

require(ggplot2)
#***********************************************************************************************************************
# 1 Visualization that displays the total number of movies for each decade
#***********************************************************************************************************************
# qplot(year,data = movies, geom = "histogram",  binwidth = 10)

#ggplot(movies, aes(x = year)) + geom_histogram(binwidth = 10, fill = "lightblue", color = "black")
ggplot(movies, aes(x = year)) + geom_histogram(binwidth = 10, fill = "blue", color = "black") + labs(title = "Number of movies per Decade", x = "Year", y = "Count")

#***********************************************************************************************************************
# 2
#***********************************************************************************************************************
# Average IMDB user rating for different genres of movies
# will have to subdivide the data set based on genres first
# Code obtained from #https://raw.githubusercontent.com/yosukekatada/msan622/9df534c38d8d635dc12849195763b8ccb9aa886b/homework1/MSAN622_HW1.R
# Code creates a new column 'genre' and then fills it based on the genre in the columns allocated to genre (18-24)
# This makes it much easier to display the results

genre <- rep(NA, nrow(movies))
count <- rowSums(movies[, 18:24])

genre[which(count > 1)] = "Mixed"

genre[which(count < 1)] = "None"
genre[which(count == 1 & movies$Action == 1)] = "Action"
genre[which(count == 1 & movies$Animation == 1)] = "Animation"
genre[which(count == 1 & movies$Comedy == 1)] = "Comedy"
genre[which(count == 1 & movies$Drama == 1)] = "Drama"
genre[which(count == 1 & movies$Documentary == 1)] = "Documentary"
genre[which(count == 1 & movies$Romance == 1)] = "Romance"
genre[which(count == 1 & movies$Short == 1)] = "Short"
movies$genre<-as.factor(genre)

a = aggregate(rating~ genre, movies, mean)
a

ggplot(a, aes(x= reorder(genre, rating), y = rating)) + geom_bar(stat = "identity", fill = "lightblue", color = "black") + labs(title = "Average IMDB rating per Genre", x = "Genre", y = "Rating")

# change over time
action.movies = subset(movies, genre == "Action")
action.movies = aggregate(rating~year, action.movies, mean)

ggplot(action.movies, aes(x= year, y = rating, group = 1)) + geom_line(stat = "identity", size = 1, color ="red")+ labs(title = "Change in Average IMDB rating per Action Genre over time")

action.movies = subset(movies, genre == "Action")
action.movies = aggregate(rating~year, action.movies, mean)

#ggplot(action.movies, aes(x= year, y = rating, group = 1)) + geom_line(stat = "identity", size = 1, color ="red")+ labs(title = "Change in Average IMDB rating per Action Genre over time")


nlevels(movies$genre)



for (s in levels(movies$genre)){

    r.movies = subset(movies, genre == s)
    #print (s)
    r.movies = aggregate(rating~year, r.movies, mean)

    p= (ggplot(r.movies, aes(x= year, y = rating, group = 1)) + geom_line(stat = "identity", size = 1, color ="blue") + labs(title = cat("Change in Average IMDB rating per ", s, " Genre over time")))
    print (p)
}


#***********************************************************************************************************************
# 3 Length of move and movie rating
#***********************************************************************************************************************
# will first do a scatter plot
# will take out movies of lenght greather than 300 minutes
l.movies = subset(movies, length < 300)

l.movies = aggregate(rating~length, l.movies, mean)
#l.movies

ggplot(l.movies, aes(x= length, y = rating, group = 1)) + geom_line(stat = "identity", size = 1, color ="green") + labs(title = "Relationship between length of movie and rating")


l.movies = subset(movies, length < 300)
h = lm(rating ~ length, l.movies)

summary(h)

#********************************************************************************************
# R squared is 0.0014, therefore there is no association between length of movie and ratings


#***********************************************************************************************************************
# 4. Length of movie and Genre
#***********************************************************************************************************************
l.movies = subset(movies, length < 300)
l.movies = aggregate(length~genre, movies, mean)
ggplot(l.movies, aes(x = reorder(genre, length), y = length, group = 1 )) + geom_bar(stat = "identity", fill = "pink", color ="darkgreen")+ labs(title = "Relationship between length of movie and Genre")

l.movies = subset(movies, length < 300)
h = lm(rating ~ genre, l.movies)

summary(h)
#********************************************************************************************
# R squared is 0.07595, therefore there is no association between length of movie and genre


#***********************************************************************************************************************
#5 Predictors of Total number of votes
#***********************************************************************************************************************
t.movies = subset(movies, !is.na(budget))
t1 = lm(votes~year+length+budget+genre+mpaa, t.movies)
summary(t1)

t2 = lm(votes~budget+genre+mpaa, t.movies)
summary(t2)

t3 = lm(votes~budget+mpaa, t.movies)
summary(t3)

#********************************************************************************************
# Strong predictors of votes are budget and mpaa Rating of "R"

# mostly negative predictors include Genre

