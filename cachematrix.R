## This programming assignment 2 calculates the inverse of a matrix, first checking if there is a  
## value stored in the cache

## makeCacheMatrix stores list("special vector") of functions for get, set, getsolve, setsolve.
## This enables cache of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m
        
        ##list of four functions
        list(set = set, get = get, setsolve = setsolve,getsolve = getsolve)
}

## cacheSolve Checks if a value already exists for getsolve (from makeCacheMatrix) and else calculates 
## solve(inverse of matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}