## makeCacheMatrix will create a "matrix" that is really a collection of functions
## It creates a list that has four functions
## set() sets the matrix
## get() returns the matrix
## setinv() set the inverse of the matrix
## getinv() returns the inverse of the matrix
## You create the "matrix" by invoking makeCacheMatrix and giving the matrix as the argument
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list( set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve will return the inverse of the "matrix" created with makeCacheMatrix()
## The first time it is called, the inverse is computed and is cached.
## Any subsequent calls will return the cached result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv ()
    if (!is.null (inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get ()
    inv <- solve (data, ...)
    x$setinv (inv) 
    inv
}
