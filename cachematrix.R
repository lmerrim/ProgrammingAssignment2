## R Programming Assignment 2

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. A pair of functions are presented that cache the inverse 
## of a matrix.

## Creates a special "matrix" object that can cache its inverse.
#       1. Set the value of the matrix.
#       2. Get the Value of the matrix.
#       3. Set the value of the inverse of the matrix.
#       4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        
        x$setinv(m)
        return(m)
}