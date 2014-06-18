#This file contains two functions
## One function creates a matrix and returns its inverse
## the other function computes the inverse of the matrix created above


## here we create a matrix and cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  inv<- NULL 
  set <- function(y) {
 x <<- y
   inv<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## here we return the inverse of the matrix. 
## if the inverse is cached we return from the cache.
## otherwise we recalculate
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setinv(inv)
  inv
}