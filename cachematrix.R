## Gorav Sharma

## Here we have 2 functions makeCacheMatrix and cacheSolve
## makeCacheMatrix contains set, get, setsolve and getsolve

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##
## This part is to get the cache data
cacheSolve <- function(x, ...) {
  inverse <- x$getsolve()
  if(!is.null(inverse)) {           # Check whether inverse is NULL 
    message("getting inversed matrix")
    return(inverse)                 # Returns NULL value
  }
  data <- x$get()
  inverse <- solve(data, ...)       # Calculates the inverse value
  x$setsolve(inverse)
  inverse                           # Returns inverse of 'x' matrix
}
