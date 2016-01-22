## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  inverse  <- NULL
  set <- function(n) {
    m <<- n
    inverse <<- NULL
  }
  get <- function() m
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(v, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- v$getinverse()
  if(!is.null(inverse)) 
  {
    ## Inverse already calculated, value not stale
    message("getting cached data")
    return(inverse)
  }
  
  data <- v$get()
  inverse <- solve(data, ...)
  v$setinverse(inverse)
  inverse
}