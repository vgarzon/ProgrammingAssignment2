## Coursera R Programming
## Programming assignment 2
## Victor Garzon 2016-01-12

## Sample R functions to cache potentially time consuming calculations.
## Based on Prof. Peng's makeVector and cachemean functions.

## - - - - - - - - - - - - makeCacheMatrix - - - - - - - - - - - -
## Creates a special "matrix" object that can cache its inverse.
##
## x: matrix
##
## returns list with four functions
##      'set'      sets value of cache matrix
##      'get'      gets value of cache matrix
##      'setsolve' sets value of matrix inverse
##      'getsolve' gets value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## - - - - - - - - - - - - cacheSolve - - - - - - - - - - - -
## Compute inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.
##
## x: cache matrix created with makeCacheMatrix
##
## returns calculated or cached inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
