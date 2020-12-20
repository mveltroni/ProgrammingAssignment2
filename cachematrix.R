## R script created by M. Veltroni 20 december 2020
## This function creates a special "matrix", which is really a list containing functions 
## to set the value of the matrix, to get the value of the matrix, to set the Inverse of the matrix (caching)
## and to get the Inverse of the matrix (take from the cache instead of recalculating).

makeCacheMatrix <- function(x = matrix()) {
  MatInv <- NULL
  set <- function(y) {
    x <<- y
    MatInv <<- NULL
  }
  get <- function() x
  setInv <- function(calcInv) MatInv <<- calcInv
  getInv <- function() MatInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function get the Inverse of the special matrix created with makeCacheMatrix.
## If the inverse taken from function getInv is not Null then the cached matrix is returned;
## if the inverse is Null then it is calculated using solve() function, stored in cache using setInv() and returned.
## The function print a message to explain if the inverse is calculated or taken from the cache.

cacheSolve<-function(x, ...) {
  In<-x$getInv()
  if (!is.null(In)){
    message("getting chached Inverse")
    return(In)
  }
  data<-x$get()
  In<-solve(data,...)
  x$setInv(In)
  In
}

## Below an example of code usage
## source("cachematrix.R")
## x<-makeCacheMatrix(matrix(c(3,0,2,2,0,-2,0,1,1), nrow = 3, ncol = 3))
## cacheSolve(x) #first time calculate and return the inverse
## cacheSolve(x) #second time take the inverse from cache and return the inverse