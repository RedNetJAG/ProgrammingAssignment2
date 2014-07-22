## R Programming: Programming Assignment2: Caching the Inverse of a Matrix
## functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## returns a list of functions to set an get the matrix and inverserse matrix

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


## cacheSolve returns the inverse matrix created with makeCacheMatrix
## If the inverse has already been calculated retuns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
