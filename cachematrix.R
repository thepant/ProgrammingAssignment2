## My functions are used for caching the inverse of a matrix so as to reduce computational time by 
## computing the matrix one after the other.

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse
## has been calculated, cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
