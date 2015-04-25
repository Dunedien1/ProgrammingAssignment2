##just seting up the program - creates the matrix and stores the important variables
##... on multiple levels using the <<- function as described below...
## Sets the functions for inversion later on


makeCacheMatrix <- function(x = numeric()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse 
  ##1st important note, setting m <<- inverse#... allows inverse to be...
  ##...used on multiple levels, not just the parent level. #it's what...
  ##...allows it to be used in the casheSolve below.
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

##1st checks to see if inversion has already been done, tells if done...
##... and returns the completed inverted matrix
##If not already inverted, inverts it using setinverse and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}