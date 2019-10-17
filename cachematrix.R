## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  
  ## return list of functions for matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # compute inverse of matrix 
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
  # return inverse of matrix
  return(inv) 
}
