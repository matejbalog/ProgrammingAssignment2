## Functions for manipulating matrices that allow for their inverses to be
## cached, saving computation time if the inverse of a matrix is queried more
## often than the matrix is modified. The makeCacheMatrix function builds such a
## matrix and the cacheSolve function returns its inverse.

## Converts a standard matrix x to a matrix that allows for its inverse to be
## cached. The original matrix x is unmodified, the converted matrix is returned
## by the function.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Takes a CacheMatrix x and returns its inverse. If the inverse is cached, no
## recomputation will be required.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
