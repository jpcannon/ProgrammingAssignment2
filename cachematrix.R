## Coursera: rprog-031
## Programming Assignment 2
## Author: Joe Cannon
## Date  : 08/21/2015

## The following is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(the_matrix = matrix()) {
  mmatrix <- NULL
  set <- function(y) {
    the_matrix <<-y
    mmatrix <<-NULL
  }
  get <- function() the_matrix
  setinverse <- function(inverse) mmatrix <<- inverse
  getinverse <- function() mmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(cMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- cMatrix$getinverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  data <- cMatrix$get()
  iMatrix <- solve(data, ...)
  cMatrix$setinverse(iMatrix)
  iMatrix
}


m <- matrix(c(1,3,3,5),ncol=2,nrow=2)
m
cm <-makeCacheMatrix(m)
cm
im <-cacheSolve(cm)
im
im <-cacheSolve(cm)
im