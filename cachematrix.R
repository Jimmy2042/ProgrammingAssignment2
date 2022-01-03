## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Computes the inverse of the matrix cached and prints the result

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached matrix")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
     
}