## These functions work together to provide the capability to compute the inverse
## of a matrix, caching the result so that the inverse is not computed every time
## the function is called

## makeCacheMatrix creates a list object that provides functions for
## getting and setting the underlying matrix and the inverse
## example:
##   m <- makeCacheMatrix(matrix(runif(5^2),5,5))
##   m$get  ## returns the embedded matrix
##   m$set(matrix(runif(4^2),4,4))  ## sets a new embedded matrix
##
##   m$getinverse() and m$setinverse() are used by the cachesolve function
##    to retrieve and store the cached result                

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(f) m <<- f
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', caching the result so that
## the inverse won't be re-computed if the function is called a second time
## on the same "cache matrix" object
## The first argument to this function is the "cache matrix" object produced
## by the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
