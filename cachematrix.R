## Matrix inversion can be a costly function.  These methods will allow for
## the inverse of a matrix to be cached so that it need not be recalculated.

## makeCacheMatrix - creates a list of functions that are capable of caching
## the inverse of the supplied matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - attempts to look up the inverse of the matrix x if it is cached.
## if it is not cached, will calculate the inverse, cache it, and return the
## value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
