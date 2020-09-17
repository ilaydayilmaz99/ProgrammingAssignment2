## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object which can cache its inverse.

makeCacheMatrix <- function(k = matrix()){
    inverses <- NULL
    set <- function(m) {
        k <<- m
        inverses <<- NULL
    }
    get <- function() k
    setinverse <- function(inverse) inverses <<- inverse
    getinverse <- function() inverses
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(k, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverses <- k$getinverse()
  if(!is.null(inverses)) {
    message("getting cached data")
    return(inverses)
  }
  data <- k$get()
  inverses <- solve(data, ...)
  k$setinverse(inverses)
  inverses
}
