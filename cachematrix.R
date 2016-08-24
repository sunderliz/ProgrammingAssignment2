## These functions work together so that once you have found the inverse of a matrix 
## you will not have to waste your processor's time finding it again, because it will 
## hold it in a cache. In order to cache another matrix's inverse you don't have to 
## run throught the whole process again, but can skip the makeCacheMatrix step and just 
## replace the original matrix with another by calling originalMatix$set(anotherMatrix).

## The "makeCacheMatrix" function creates a "matrix" that is really a list of four functions
## which can cache its inverse by taking advantage of the properties of parent environments
## in R.


makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(solve) inv <<- solve
                getinv <- function() inv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## We can input the "matrix" list result of the makeCacheMatrix function into
## this new function. This function will determine if there is already a calculated 
## inverse in the cache. If there is it will be used. If not, the inverse will be
## calculated using the solve function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                      return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
