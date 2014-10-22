##
## These R functions are able to cache potentially time-consuming
## computations in matrix.
##

###############################################################################
## Description :
## Function "makeCacheMatrix" is used to create a special "matrix", which
## contains a list of functions to operate input matrix, x.
##
## Input :
## x - a matrix
##
## Return :
## a list of functions to operate input matrix, x.
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    if(!all(x == y)) {
      x <<- y
      inv <<-NULL      
    }
  }
  get<-function()x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)  
}


###############################################################################
## Description :
## Function "cachesolve" calculates the inverse of the special "matrix" created
## with function "makeCacheMatrix". The function first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the inverse in the cache via the setinv function.
##
## Input :
## x - a list of functions return from function "makeCacheMatrix"
## Return :
## an inverse matrix
###############################################################################
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get();
  inv<-solve(data,...)
  x$setinv(inv)
  inv  
}
