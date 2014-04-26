
#############################
## Programming Assignment 2
## PROG:  cachematrix.R
## User:  cjesse01
#############################

################
## PROG Overview
## This program contains two functions:  makeCacheMatrix and cacheSolve
## The two programs together allow the caching of a matrix inverse for reuse,
##  rather than computing the inverse repeatedly.
################

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,  # create the list of functions
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getsolve()           # Query the x Matrix cache
        if(!is.null(m)) {           # If there IS a cache,
                message("getting cached matrix inverse")
                return(m)           # then return the cache, rather than compute
        }
        data <- x$get()             # If there IS NO cache (the else),
        m <- solve(data, ...)       # then compute here
        x$setsolve(m)               # Save the result back to the x cache
        m                           # Print the result
}