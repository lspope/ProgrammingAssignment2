## These two functions can be used to cache the inverse of a square,
## invertable matrix.


## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseofx <- NULL
    set <- function(y) {
        x <<- y
        inverseofx <<- NULL
    }
    get <- function() x
    setinverse <- function(theinverse) inverseofx <<- theinverse
    getinverse <- function() inverseofx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by 
## the makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseofx <- x$getinverse()
    if(!is.null(inverseofx)) {
        message("getting cached data")
        return(inverseofx)
    }
    data <- x$get()
    inverseofx <- solve(data, ...)
    x$setinverse(inverseofx)
    inverseofx
}
