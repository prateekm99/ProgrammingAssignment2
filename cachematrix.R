## Matrix inverse computation is a costly operation, these functions/objects will
## allow us to compute matrix inverse as well as store them in cache, so that 
## the next time we need a matrix inverse we can simply get it from cache
## instead of recomputation

## Takes an invertible matrix and return a complex
## object which contains functions for storing
## its value and inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Take a makeCacheMatrix object as input,
## compute the matrix invers if it is not alread in cache,
## and return the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}
