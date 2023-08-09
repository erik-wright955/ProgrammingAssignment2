##Programming Assignment 2: Lexical Scoping
##Assignment: Caching the Inverse of a Matrix

##Write the following functions: 
##makeCacheMatrix: This function creates a special "matrix" object that 
##can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

##Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##Function to compute the inverse of the special matrix and cache it
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ##Return a matrix that is the inverse of 'x'
  inv
}
