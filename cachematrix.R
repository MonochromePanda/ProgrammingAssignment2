## Put comments here that give an overall description of what your
## functions do
## Attempting to cache the inverse of a Matrix. 
## A Matrix Inversion takes a lot of time to compute. 
## We will save time by caching the inverse, so we do not need to compute it 
## over and over again. 
## Below is going to be a set of functions. They will create an object that 
## stores the matrix, and as we intend, to cache the inverse.

## Write a short comment describing this function
## This is a function that is going to create a matrix object that can cache the
## inverse of itself. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }

## Write a short comment describing this function
## This function outputs the inverse of the "special" matrix by the 
## makeCacheMatrix, as seen above. If we see the inverse is already calculated, 
## then the cacheSolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
      message("getting the cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
