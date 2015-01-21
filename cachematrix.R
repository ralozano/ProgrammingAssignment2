## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## Abstract Data Type (ADT) that is used to cache matrix inversion
## It returns a list of functions which are used to create the ADT, get the matrx and its inverted

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  minver <- NULL
  set <- function(y) {
    x <<- y
    minver <<- NULL
  }
  get <- function() x
  
  setminver <- function(inver) minver <<- inver
  getminver <- function() minver
  list(set = set, get = get,
       setminver = setminver,
       getminver = getminver)
} 


## x represents a list of functions ADT, (see makeCacheMatrix above)
## Before calculate the matrix inversion, this function look up in the cache to try to answer 

cacheSolve <- function(x, ...) {
    minver <- x$getminver()
    if(!is.null(minver)) {
      message("getting cached inverted matrix")
      return(minver)
    }
    matrix <- x$get()
    minver <- ginv(matrix, ...)
    x$setminver(minver)
    minver
}
