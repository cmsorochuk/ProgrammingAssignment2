## These functions are used to calculate and cache inverse matrix calculations
## To save processing time, if a matrix inverse has already been calculated, it will be cached.
## Before an inverse is calculated, check to see if it already exists in cache.  If so, retrieve.  If not, calculate.

## This first function, 1) Sets the value of the vector, 2) Gets the value of the vector,
## 3) Sets the value of the inverse, and 4) Gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(newValue) {
      x <<- newValue
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This second function is the one that checks for an existing inverse.  If none exists, it calculates/returns the inverse.

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
