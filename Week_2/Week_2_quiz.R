#Question 1 vector that contains 20 numbers

v = as.integer(rnorm(20, 10,2))
v
class(v)
mode(v)

#Question 2, convert vector to character
v = as.character(v)
class(v)

#Question 3, convert to factor
v = as.integer(rnorm(20, 10,2))
v = as.factor(v)
class(v)


# Question 4. Levels of factor
v
levels(v)
length(levels(v))


#Question 5 using the formula 3x^2 -4x +1
v  # v is a factor, will have to convert it back to character and then numeric before we can use it..
v = as.numeric(as.character(v))
v

sapply(v, function(x) 3*(x^2) - 4 * x + 1)

#6 

v1 = c(rep(1,8))
v2 = c(5,4,6,2,3,2,7,8)
v3 = c(8,9,4,7,4,9,6,4)
v4 = c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1)

x = cbind(v1,v2,v3)
x
y = cbind(v4)
y

#(solve(crossprod(x)) %*% t(x)) %*% y


solve(crossprod(x)) %*% crossprod(x,y)

# to test resuts
fit <- lm(y ~ x-1)
fit

ls = lsfit(x,y, intercept = FALSE)
ls


#7 Create a named list 
# will use car manfactures and car names

nlist = list(BMW ="X5", Toyota = "Corolla", Jeep = "Cherokee")
nlist



#Question 8
n = c(1:10)
d = c("a", "g","h","p","n","q","w","s","m","v")

f = c("Good", "Better",  "Better", "Best","Best","Good", "Good", "Better",  "Better","Better")
f = factor(f)
levels(f)
mydate = as.integer(rnorm(10,50,10)) + Sys.Date() 
mydate
mydframe = data.frame(d,f,n,mydate, stringsAsFactors = FALSE)
is.factor(mydframe[,"f"])
levels (mydframe[,"f"])
str(mydframe)


#Question 9

#Add new factor level Excellent

#f = factor(f, levels = c(levels (f), "Excellent"))
mydframe[,"f"] = factor(mydframe[,"f"], levels = c(levels(mydframe[,"f"]), "Excellent"))
mydframe[,"f"]                         
                     
mydframe = rbind(mydframe, c("a","Excellent",20,"2015-1-31"))
mydframe

# 10. Show the code that would read in a CSV file called temperatures.csv from the current working directory.

#get current working directory
getwd()
myfile = read.csv("temperatures.csv", header = TRUE, skip = 11) # make sure file exists though
summary(myfile)


# 11. Show the code that would read in a TSV file called measurements.txt #other than the working directory on your local machine.

mytxtfile = read.table("C:/Users/Jare/Desktop/measurements.txt", header=TRUE, sep ="", fill = TRUE)
summary(mytxtfile)

        

# 12. Show the code that will read in a delimited file with a pipe separator (the "|" symbol) from a website location. 
#(You may make up an appropriate URL.)

# ref http://www.portfolioprobe.com/user-area/documentation/portfolio-probe-cookbook/data-basics/read-a-tab-separated-file-into-r/
filePipe <- read.table("http://www.portfolioprobe.com/R/blog/xassetCountrySector.txt", sep="|", header=TRUE)
# Note the file is tab seperated "\t", I could not find a file that was pipe "|" seperated online
head(filePipe)


#Question 13 code to calculate the 12 factorial

myfactorial = function (n){
  
  
  if (n == 1) {
    return (n)
  }
  
  else {
    
    return (n * myfactorial(n-1))
  }
}  

myfactorial(12)
  
#14 Loop to calcualate final balance , rounded to the nearest cent in an account that earns 3.24% interest compounded montly after six years
# if the original balance is $1500

# P = principal amount (the initial amount you borrow or deposit)
# r  = annual rate of interest (as a decimal)
# t  = number of years the amount is deposited or borrowed for.
# A = amount of money accumulated after n years, including interest.
# n  =  number of times the interest is compounded per year  

# A = P (1+ (r/n))^(nt)

# from https://qrc.depaul.edu/StudyGuide2009/Notes/Savings%20Accounts/Compound%20Interest.htm

P = 1500
r = 0.0324
t = 6
n = 12

A = P *(1 + (r/n))^(n*t)
round(A, digits = 2)



#15 Sum of every third element in a 20 number array

num = c(1:20)


sum = 0
x =0
repeat
{
  
  x = x + 3
  if (x> length(num)) break
  sum = sum + num[x]
  
}
     
sum




#16 Use a for loop to calculate the sum for x^i for i = 1 to 10 for the value of x = 2


x = 2
sumx = 0
i= 1

for (i in 1:10){
  
  sumx = sumx + x^i
  
}

sumx


#17 Use a while loop to calculate the sum for x^i for x = 1 to 10 for the value of i = 2

x = 2
sumx = 0
i = 1

while (i < 11){
  
  sumx = sumx + x^i
  i =i+1

}
sumx

#18 Without using a loop calculate the sum for x^i for i= 1 to 10 for the value of x = 2


x = 2

i = c(1:10)

sumy = sum(c(x^i))


sumy


#19

nvector = seq(20,50,5)

#20

vchar = rep("example",10)
vchar

#21 Show how to take a trio of numbers a,b and c and implement the quadratic equation

myQuad = function (a, b, c) {
  #   ax^2+bx+c=0  from http://en.wikipedia.org/wiki/Quadratic_equation
  x1 = 0
  x2 = 0
  errorFound = TRUE
  tryCatch({
  
    x1 = (-b + sqrt(b^2 - 4*a*c))/2*a
    x2 = (-b - sqrt(b^2 - 4*a*c))/2*a
    errorFound = FALSE},
    
    error = function(e){})
    
    
  if (errorFound) 
     
     return ("Error in inputs")
   
   
  else 
    
    return (c(x1, x2))
  
  
}  

myQuad(7,8,-2)


