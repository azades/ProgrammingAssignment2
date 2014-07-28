## These two functions “cacheSolve” and “makeCacheMatrix”
## calculate the Inverse of a matrix and cache it in order to
## not computed repeatedly.

## makeCacheMatrix create and return a new object that can cache its inverse.
## makeCacheMatrix does following steps:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## The “cacheSolve” function checks if the inverse has been computed.
## If there is, it gets the result and skips the computation again.
## If there is not, it computes the inverse, sets the value in the cache.

cacheSolve <- function(x=matrix(), ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
    
}
