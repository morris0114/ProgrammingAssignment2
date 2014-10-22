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
  # initialize local variable "inv" as NULL.
  inv<-NULL

  #######################################################################
  ## Description :
  ## Function "set" is used to set matrix y into variable, x
  ## 
  ## Input :
  ## y - a matrix
  #######################################################################
  set<-function(y) {
    # check whether the input matrix, y, is same as x or not.
    if(!all(x == y)) {
    # y is not the same as x and set x as y
      x <<- y
    # clear value of variable "inv"
      inv <<-NULL      
    }
  }

  #######################################################################
  ## Description :
  ## Function "get" is used to get matrix "x"
  ## 
  ## Return :
  ## a matrix
  #######################################################################
  get<-function()x

  #######################################################################
  ## Description :
  ## Function "setinv" is used to set matrix "inverse" into
  ## variable "inv"
  ## 
  ## Input :
  ## a matrix
  #######################################################################
  setinv<-function(inverse) inv<<-inverse # set inversed matrix

  #######################################################################
  ## Description :
  ## Function "getinv" is used to get value of inverse matrix "inv"
  ## 
  ## Return :
  ## an inverse matrix
  #######################################################################
  getinv<-function() inv

  # return a list of functions defined in function "makeCacheMatrix"
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
## an inversed matrix
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
