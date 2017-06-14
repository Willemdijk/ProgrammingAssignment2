## The functions described below will in short create a Matrix vector and cache its inverse.
## More detailed explanations is given below.

## This function for example creates a matrix object and makes it possible that its inverse is called upon by the second function.

makeCacheMatrix <- function(x = matrix()){ #initializes object x
  m <- NULL # initializes object m
  set <- function(y){ 
    x <<- y #value of input argument assigned to x object in parent environment
    m <<- NULL #value of NULL set to m object in parent environment
    }
  get <- function() x #getting value of x from parent environment
  setsolve <- function(solve) m <<- solve #Setsolve defines the setter for the inverse m.input argument assigned to value of m in parent environment
  getsolve <- function() m #Getsolve defines the getter for the inverse m.
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) #Then a list is created in which all the functions are given a name.
 }

## This function checks whether a given object already has an inverse value. If yes then value retrieved. If not inverse calculated and cached.

cacheSolve <- function(x, ...){ #argument of type makeCacheMatrix can be provided here.
  m <- x$getsolve() #calls getsolve on input object.
  if(!is.null(m)){ #check whether m is not NULL.
    message("getting cached data")
    return(m) #if value is not NULL but an inverse, then cache is retrieved and returned.
  }
  data <- x$get() #As a name was given to the function in the list in the makeCacheMatrix, the function can now be extracted to get value vector.
  m <- solve(data, ...) #The inverse of the object given as argument is calculated when this was not yet the case.
  x$setsolve(m) #Inverse is cached.
m #inverse is printed.
}
## ## The functions outlined above can be tested by using the vectors below.
# Test Matrices originally given by Alan E Berger, Mentor. 
# R programming Week 3 forum.
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol =2)
n1 <- matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)
#Both vectors above are each other's inverses.
#Testing can be done in the following manner.
myMatrix_object <- makeCacheMatrix(m1)cacheSolve(myMatrix_object)
#This would give the result:
#      [,1] [,2]
# [1,]    6    8
# [2,]    2    4
#This shows that both functions work as providing m1 as argument to makeCacheMatrix
#provides its universe when used by cacheSolve.
