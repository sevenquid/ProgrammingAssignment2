## This pair of functions will return the inverse of a matrix.
## If the inverse has not been calculated previously, it 
## calculates it from scratch and caches the result. 
## If the inverse has been calculated previously, it returns 
## the cached inverse instead of calculating it again.

## makeCacheMatrix creates a list of 3 functions: 
## get() is just the original matrix
## setinv() caches the value of the inverse
## getinv() gets the cached value of the inverse or returns
##  NULL if not cached

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    
    list(get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve first looks for a cached inverse. If it finds
## one, it returns that cached value. If it doesn't find one,
## it calculates the inverse, then caches and returns it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
