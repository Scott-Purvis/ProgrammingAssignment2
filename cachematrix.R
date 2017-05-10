## Doing long or repeateding long computation in R 
## can be very time-consuming.  Under these conditions,
## it would be desirable to cache the results of these 
## computations (seperate environment) and call them when
## needed rather than recomputed.


## To do this, 2 functions are needed.

## makeCacheMatrix is a function that creates a list as an output.

## This list contains the following:
##  - variable that sets the value of the vector (set)
##  - variable that gets the value of the vector (get)
##  - variable that sets the value of the inverse matrix (setim)
##  - variable that gets the value of the inverse matrix (getim)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setim <- function(solve) i <<- solve
  getim <- function() i
  list(set = set, get = get,
       setim = setim,
       getim = getim)
}

## cacheSolve is a function that checks to see if the 
## inverse matrix calculation is stored in the cache,
## and if so, "gets" the inverse matrix from the cache
## and skips the calculation.  Otherwise, it calculates
## the inverse matirx.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getim()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setim(i)
  i
}