## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Below you can see a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    k <- x$getInverse()
    if(!is.null(k)){
      message("getting cached data")
      return(k)
    }
    v <- x$get()
    k <- solve(v,...)
    x$setInverse(k)
    k
}
