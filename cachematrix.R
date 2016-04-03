## Author: Emily Quinn Finney
## Programming Assignment 2, Introduction to R
## This module creates a matrix with caching capability.

## Creates a list of functions that can store the values of a matrix
## and of its inverse, and can retrieve these values when called upon.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                  x <<- y
                  inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Creates a matrix-solver that can retrieve the value of a matrix created
## by makeCacheMatrix (if it has been used), and solves for the inverse
## otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}