## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Answer of Q 1 "makeCacheMatrix"
##The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
##The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
##The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
##in order to retrieve the inverse from the cached matrix that is stored in the makeCacheMatrix() object's environment.

## makeCacheMatrix
# The first function, makeCacheMatrix, creates a special "matrix", 
# which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solvematrix) inv <<- solvematrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse)
}

## Write a short comment describing this function

## Answer of Q 2 "cacheSolve"
## cacheSolve
# The following function calculates the inverse of the special "vector" created with makeCacheMatrix. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
