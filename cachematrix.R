## Coursera: rprog-031
## Programming Assignment 2
## Author: Joe Cannon
## Date  : 08/21/2015

## The following is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## It takes a Matrix stores it and returns a function vectore that can
## be used to store retreive both original and inverse
makeCacheMatrix <- function(the_matrix = matrix()) {
  mmatrix <- NULL
  ## Method to store the Matrix
  set <- function(y) {
    the_matrix <<-y
    mmatrix <<-NULL
  }
  ## Method to get the Matrix
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
  ## Call the cMatrix's getinverse method to return 
  ## a matrix that is the inverse of 'x'
  iMatrix <- cMatrix$getinverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  ## Otherwise calculate the inverse
  data <- cMatrix$get()
  iMatrix <- solve(data, ...)
  ## Call the cMatrix's setinverse method
  cMatrix$setinverse(iMatrix)
  iMatrix
}

## The following is values and calls used to test


m <- matrix(c(1,3,3,5),ncol=2,nrow=2)
m
cm <-makeCacheMatrix(m)
cm
im <-cacheSolve(cm)
im
im <-cacheSolve(cm)
im