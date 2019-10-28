## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Answer of Q 1 "makeCacheMatrix"

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
