## Put comments here that give an overall description of what your
## functions do
## Function makeCacheMatrix creates a special matrix object that 
## can cache its invers in order to reduce the computation. 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  setMat <- function(y) {
    x <<- y
    mat <- NULL
  }
  getMat <- function() x
  setInv <- function(solve) mat <<- solve
  getInv <- function() mat
  list(setMat = setMat, getMat = getMat,
       setInv= setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the special matrix returned by 
## the function makeCacheMatrix. If the inverse is already in the memory,
## it returns it from the cache and doesn't calculate it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getInv()
  if(!is.null(mat)) {
    message("Getting cached data")
    return (mat)
  }
  data <- x$getMat()
  mat <- solve(data, ...)
  x$setInv(mat)
  mat
}
