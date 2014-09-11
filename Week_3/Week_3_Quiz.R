#Question 1
#Write a function that takes a numeric vector and calculates the mean of the observations in the vector
myMean = function (x){
  return (sum(x)/length(x))
  }
myVec = runif(10,1,10)
myMean(myVec)

#Result
#myMean(myVec)
#[1] 4.882776
#mean(myVec)
#[1] 4.882776

# 2. Modify your function in the previous question so that it can handle a numeric vector with missing values.
myMean2 = function (x){
  # Take out the NAs
  x = x[!is.na(x)]
  #then calculate the mean as before
  return (sum(x)/length(x))
}

#create a new numerica vector
myVec = runif(10,1,10)
#add some NA's to our vector
mySample = sample(1:10, 3)
myVec[mySample] = NA

# check to see that the vector has NAs
myVec
#Result
#8.831992 6.248558       NA       NA 7.304826 8.927194 8.859061 9.510656 5.472828       NA
myMean2(myVec)
#check to see if the ans is correct

mean(myVec, na.rm = TRUE )


# 3. Write a function that takes two numeric input values and calculates the greatest common divisor of the two numbers.
# we are going to assume that two positive integers have been supplied, and therefore are not going to error check
fun3  = function (x, y){
n = ifelse((x>=y), y, x )
flag = TRUE

while (flag == TRUE) {

    if ((x %% n == 0) && (y %% n == 0))

      {
        return (n)
      } else
      {
          n = n - 1
      }
  }
}

print (fun3(210, 45))
# Result
# [1] 15

# 4. Write a function that implements Euclid's algorithm (you may need to do a bit of research to find this algorithm)
# for finding the greatest common divisor of two numeric inputs.
# using the Euclid's algorithm and recursion

fun4  = function (a, b){
largeN = ifelse((a > b), a, b )
smallN = ifelse((a > b), b, a )

  if (largeN %% smallN == 0)

      {
        return (smallN)
      }
  else
      {
        return (fun4(smallN, (largeN %% smallN)))
      }
}

print (fun4(45,210))
# Result
# [1] 15

# 5. Write a function that takes two numeric inputs x and y and calculates X^2.y =2.x.y -x.y^2

myFun5 = function(x, y){

  return (((x^2)*y)+ (2*x*y) - (x*(y^2)))

}

myFun5(5,8)
# Result
#[1] -40

# 6. Read in the week-3-price-data.csv and week-3-make-model-data.csv files as data frames and then merge them
# by the ModelNumber key. Leave the "all" parameters as their defaults. How many observations end up in the result?
# Is this what you would have expected?

fileLoc = "C:/Users/jare/SkyDrive/WorkDocs/CUNY/607/Week 3/"

price_data = read.csv(paste(fileLoc,"week-3-price-data.csv", sep =""))
model_data = read.csv(paste(fileLoc,"week-3-make-model-data.csv", sep =""))

head(price_data)
head(model_data)
newData = merge(price_data, model_data, by = "ModelNumber")
head(newData)
price_data
model_data
c(paste("Number of observations =", nrow(newData), sep = " "))

# Yes, the number of observations is 27 due to the fact that in the price_data table the ModelNumber 23120 does not exist
# in the model_data


# 7. Use the data sets from the previous question, but this time merge them so that the rows from the price-data
# table all appear, even if there is no match in the make-model table.
newData2 = merge(price_data, model_data, by = "ModelNumber", all.x = TRUE)
nrow(newData2)
# Now we have 28 rows but the last row:
# 28       23120 12  Black   48667 11100   <NA>     <NA>   NA, does not have any information from model_data

# 8. Take your result from question 7 and subset it so that only the 2010 vehicles are included.
#using  subset

subset(newData2, Year == 2010)
# usiing which
newData2[which(newData2$Year == 2010),]

# 9. Take your result from question 7 and subset it so that only the red cars that cost more than $10,000
# are included.
# will use subset

subset(newData2, Color == "Red" & Price > 10000)

# 10. Take your result from question 9 and subset it so that the ModelNumber and Color columns are removed.
# select - drops the unwanted columns
subset(newData2, Color == "Red" & Price > 10000, select = -c(ModelNumber, Color))

# 11. Write a function that takes as input a character vector and returns a numeric vector
#with the numbers of characters in each of the elements in the original vector.



fun11 = function (w){
    return(nchar(w))
}

Fruit = c("Oranges", "Bananas", "Apples", "Blueberry")
y = fun11(Fruit)
names(y)= Fruit

y
#Result
# Oranges   Bananas    Apples Blueberry
# 7         7         6         9


# 12. Write a function that takes two character vectors of equal length and concatenates
#them element by element with a space as the separator. Have the function die gracefully if the vectors are the same length.

v1 = c("a", "b", "c","d", "e")
v2=  c("j","k", "l", "m", "n")

fun12 = function (x, y){

if (length(x) != length (y)){

    return("Character vectors are not of equal size. Cannot proceed")
  }

else {

  return (paste(v1, v2, sep = " "))

 }
}
y = fun12(v1, v2)
y
# Result
# [1] "a j" "b k" "c l" "d m" "e n"

# 13. Write a function that takes a character vector and returns the substring of three characters
#that begins with the first vowel in the string. Have the function handle gracefully substrings
#where this isn't possible.
myCharVec = function (v) {
# p = string to match
p = "[aeiou][[:alpha:]][[:alpha:]]"
# convert the string to lower characters
v = tolower(v)
z = regexpr(p,v)
li = list()
for (i in 1:length(v)){

    #print (paste("Match position is: ", z[i]))
    #print (paste("Extract string is: ", substr(v[i], z[i], z[i] +2)))

    if (z[i] < 0) {

        li = append(li, NA)
    } else{
        li = append(li, substr(v[i], z[i], z[i] +2))


    }

    }
names(li) = v
return (li)
}

vFruit = c("Apple", "Apricot", "Avocado", "Banana","Nxyngtyan", "Breadfruit", "Blackberry", "Blackcurrant", "Boysenberry")

myCharVec (vFruit)

# 14. Suppose you have a data frame where one column gives the month (in numeric format), the next gives the day,
#and the third column gives the year. Use R to create such a data frame (by hand is fine) and then add a fourth
#column with the date in date format.

m = as.integer(runif(10, 1, 12))
d = as.integer(runif(10, 1, 30))
y = as.integer(seq(1900, 2000, length.out = 10))

myDframe = data.frame(m,d,y)
str(myDframe)

# create a new column ydm for the date in date format and assign the date to it
myDframe$ymd = as.Date(paste(myDframe$m, myDframe$d, myDframe$y), "%m %d %Y")
myDframe
str(myDframe)

# 15. Illustrate the code necessary to take a string of MM-DD-YYYY format and convert it to a date.
d = "10-18-2014"
as.Date(d, "%m-%d-%Y")
# Result
# [1] "2014-10-18"

# 16. Illustrate the code necessary to take a date and extract the month of the date.

# current month from today
format(Sys.Date(), "%m")
# Result
#[1] "09"
# from 2009-10-28
format(as.Date("2009-10-28"), "%m")
# [1] "10"

# 17.Create a sequence of all of the dates from January 1, 2005, to December 31, 2014.
seq(as.Date("January 1, 2005", "%B %d, %Y"), as.Date("December 31, 2014", "%B %d, %Y"), by="day")


#finished