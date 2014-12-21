## Programming Assignment 2

## makeCacheMatrix constructs a list of methods that acts like
## an object encapsulating a matrix and it's inverse behind 
## getter and setter methods

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(m) im <<- m
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve attempts to retrieve the inverse of the matrix
## returned by the makeCacheMatrix$get method, first by 
## resolving value from makeCacheMatrix$getinverse and if not found
## by calculating the inverse with solve() method.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(x$getinverse())){
      x$getinverse()
    }
    m <- x$get()
    im <- solve(m) 
    x$setinvers(im)
    im
}
