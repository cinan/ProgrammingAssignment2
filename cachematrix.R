## Cache an inverse matrix. Create special caching object to read, write
## and make operation on input matrix.

## An interface for matrix operations.

makeCacheMatrix <- function(x = matrix()) {
  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_ <<- inverse
  getInverse <- function() inverse_
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Cache inverse matrix result.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
}