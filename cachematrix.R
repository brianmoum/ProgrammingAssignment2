## Two functions, one which takes in a numeric square matrix and stores it as a value in a list of 
## functions, which are used to set or get the matrix and its inverse. The second function takes in
## a matrix created by the first function, and either prints the stored value for the inverse, or
## calculates the inverse and stores it in the list.



## Function which has four subfunctions; one to get and one to set the stored value for a square matrix,
## and one to get and one to set the cached value of the matrix inverse. When a new value is stored
## as the matrix, the value for the inverse is set to NULL. 

makeCacheMatrix <- function(m = matrix()) {
  
  i = NULL
  
  set <- function(n) {
    m <<- n
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function which takes in a matrix defined by makeCacheMatrix and does one of two things: prints
## the value stored in getInverse if it isn't NULL, or calculates the inverse using solve() and
## stores the value in the list using setInverse. It is assumed that the matrix stored using
## makeCacheMatrix is invertible. 

cacheSolve <- function(m, ...) {
  
  i <- m$getInverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setInverse(i)
  return(i)
}


## Test calling function. A is a standard matrix, which is copied into the"special" matrix Av.
## invTest tries to retrieve the stored value for the inverse, which is NULL. Then, cacheSolve is
## ran on Av, and inv retrieves the value from getInverse again, which is now the actual inverse.

A <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol = 3, byrow = TRUE)
Av <- makeCacheMatrix(A)

invTest <- Av$getInverse()
print(invTest)

cacheSolve(Av)
inv <- Av$getInverse()
print(inv)