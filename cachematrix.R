## Put comments here that give an overall description of what your
## functions do

## These functions caches the inverse of a matrix which usually is a costly computation and therefore caching it may be a benefit in some cases

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by previous makeCacheMatrix function
## And if the inverse has already been calculated, the cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}


