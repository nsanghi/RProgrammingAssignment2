## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list which has original matrix and its inverse cached
## it has two settor and two gettor functions
##'set' to set the matrix
##'get' to get the matrix
##'setinverse' to set the inverse of the matrix
##''getinverse' to get the inverse of the matrix from cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inv <<- inver
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function tries to get the inverse of special matrix created by
## function makeCacheMatrix. It first checks for the cache result and return the same.
## If there is no cache result, it calculates the inverse and stores it for future use
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting inverse matrix from cache data")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M)
  x$setinverse(inv)
  inv
}
