##Author: Ravindra Chaudhary
##Date: Sep 17, 2016
##Project: Programming R - Week3 - Programming Assignment2
##A combination of these two functions allow to take a matrix as an argument
##and return an inverse of that matrix
##it also cache the inverse of a matrix in the memory as long as the input matrix does not change

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { #x is an input square matrix
  m <- NULL                      #m is empty matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) { #x is an input square matrix
  m <- x$getInverse()
  if(!is.null(m)) {
    message("retrieving from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

##End of the code cachematrix.R
