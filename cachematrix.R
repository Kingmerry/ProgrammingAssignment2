## Matrix inverse is usually a costly computations and we can benifit from caching
## the inverse of the computed inverse of a matrix rather than compute it repeatedly.
## So I first write a "makeCacheMatrix" function to cache the inverse of the matrix,
## and then use the returned value as the input of the "cacheSolve" function. I use
## the "cacheSolve" function to compute the the inverse of the "matri", if the iverse
## has already been calculated and cached, I can retrieve the inverse from the cache
## instead of computing again, saving a lot of time. If not computed, then simply
## compute the inverse, return it and set the inverse in the cache via the setInverse
## function.


## This function creates a special "matrix" object that can catche its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Set then mean to NULL for starters.
        inverse <- NULL
        ## Create a setter that caches v and m.
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## Create a getter that returns the cached v.
        getMatrix <- function() {
                x
        }
        ## Create a "setInverse" function.
        setInverse <- function(solve) {
                inverse <<- solve
        }
        ## Create a getInverse function that returns the mean.
        getInverse <- function() {
                inverse
        }
        ## Return the makeCacheMatrix object as a list of 4 functions.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" returned
## by the "makeCacheMatrix" above. If the iverse has already been calculated 
## (and the matrix has not been changed), then the "cacheSolve" should retrieve
## the inverese from the cache. If not, then calculate the iverse, return it and
## set the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        ## Check whether the inverse has already calculated or not.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Get the matrix.
        data <- x$getMatrix()
        inverse <- solve(data, ...)
        ## Set the inverse in the cache.
        x$setInverse(inverse)
        ## Return the inverse calculated.
        inverse
}
