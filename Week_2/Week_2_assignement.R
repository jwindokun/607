# Question 1

# 1 a Assign five individuals to a vector named queue
queue = c("James", "Mary", "Steve", "Alex", "Patricia")

# 1 b Update the queue to add a new patron Harold
queue = c(queue, "Harold")
queue

#1 c, update the quene to reflex the fact that James has checked out
queue
idx = which(queue %in% "James")
idx
queue[-idx]

queue
# can also do the same this way
# queue = queue [-which(queue %in% "James")]
# queue

# First find out the location of Steve
# 1 d. Update the queue to reflect the fact that Pam has talked her way in front of Steve with just one item
qSteve = which(queue %in% "Steve")
qSteve
# Add Pam to the newqueue
newqueue = c(queue[1:qSteve -1],"Pam")
newqueue

# add the rest of the old queue to the queue

queue = c(newqueue, queue[qSteve:length(queue)])
queue
# 1 e. Delete Harold

queue = queue [-which(queue %in% "Harold")]

# 1 f. Delete Alex

queue = queue [-which(queue %in% "Alex")]

#1 g. Position of Patricia

which(queue %in% "Patricia")
#1 h. number of people in the queue = length of queue

length(queue)

# Question 2

myQuad = function (a, b, c) {
  #   ax^2+bx+c=0  from http://en.wikipedia.org/wiki/Quadratic_equation
  x1 = 0
  x2 = 0
  
  discriminant = b^2 - 4*a*c
  
  
  #print (discriminant)
  
  if (discriminant < 0){
    
    return ("No Solution")
  }
  
  x1 = (-b + sqrt(b^2 - 4*a*c))/2*a
  x2 = (-b - sqrt(b^2 - 4*a*c))/2*a
  
  if (discriminant > 0){
    
    return (c("Two Solutions",c(x1,x2)))
  }
  
  else
    
  {
    
    return (c("One Solution",x1))
  }
  
  
}  

myQuad(1,2,1)


# Question 3
# Determine how many numbers from 1 to 1000 are not divisible by any of 3, 7 and 11
count = 0

for (x in 1:1000)
{
  
 if (x%%3 != 0 & x%%7 != 0 & x%%11 != 0)
 {
   
   
   
   count = count +1}
   
}

count

#question 4. Take there input constants f, g and h and detemine if they form a pythagorean Triple

# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }


myPy = function(f, g, h){
  
  v = c(f,g,h)
  v = v[order(v)]
  
  if ( (v[1])^2 + (v[2])^2 == (v[3])^2)
    
    {
        return(TRUE)
    }
  
  else
  
    {
        return(FALSE)
  
    }
}


myPy(10,6,8)











