## Functions
# makeCacheMatrix and cacheSolve create special datatype that knows how to create matrix and 
# its inverse and how to boost performance by calculating inverse only once when matrix is 
# created/changed

## makeCacheMatrix create special datatype of matrix that can save matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  # Matrix X inverse value inicialization
  xInv <- NULL
  
  # Setters and getters for matrix and matrix inverse
  set <- function (m){
    x <<- m
    xInv <<- NULL
  }
  
  get <- function() return(x)
  
  setInv <- function(inv) xInv <<- inv
  
  getInv <- function() return(xInv)
  
  return(list(set=set, get=get, setInv=setInv, getInv=getInv))
}


## Function cacheSolve return a matrix invere of 'x'.
# Better performance becauase of matrix inverse chaching

cacheSolve <- function(x, ...) {

  xInv <- x$getInv()
  if(!is.null(xInv)) {
    message("Cached value")
    return(x$getInv())
  }
  
  # Else - calculate and set matrix inverse and return calculated value
  x$setInv(solve(x$get()))
  return(x$getInv())
  
}


## Unit test
mt1 <- makeCacheMatrix(matrix(1:4, nrow=2))
mt1$get()

cacheSolve(mt1)
cacheSolve(mt1)
# Setting new values
mt1$set(matrix(2:5, nrow=2))
cacheSolve(mt1)
cacheSolve(mt1)
# Unit test End