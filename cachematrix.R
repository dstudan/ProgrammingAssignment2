## The following functions allow you to create a matrix,
## obtain the inverse of this matrix, and cache the inverse
## for future use.
    

## makeCacheMatrix sets and gets the value of a matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## CacheSolve checks if the inverse of a matrix has been
## calculated. If so, it gets the inverse from the cache. If not,
## it computes the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
