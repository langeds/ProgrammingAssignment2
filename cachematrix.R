## makeCacheMatrix creates a matrix that is capable of cacheing its inverse
## cacheSolve finds the inverse of the matrix either from the cache or by computing
## the inverse if no inverse is found in the cache.

## Creates and manages a matrix that can cache its inverse.
## set -- allows the matrix to be set to a new matrix value. When the value changes
##         the cached inverse is wiped clean.
## get -- returns the matrix
## setinv -- stores the inverse of the matrix in the cache
## getinv -- returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix that has been created and managed
## using makeCacheMatrix.
## If there is a cached version of the inverse, then that value is returned.
## If there is not a cached value, then the inverse is computed, stored in the cache
## and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
