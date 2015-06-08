## Implementation of a matrix solver that can cache previously computed
## inverses.
##
## Author  : Stephematician
## Created : 8/6/2015
##
## Outline provided by R.D.Peng
## https://github.com/rdpeng/ProgrammingAssignment2


## Constructs a list which contains functions capable of storing a cached
## inverse of a matrix.

makeCacheMatrix <- function(A = matrix()) {
    ## 'A' is a square invertible matrix.
  
    invA <- NULL

    get <- function() A

    set <- function(B) {
        A <<- B
        invA <<- NULL
    }

    getinv <- function() invA

    setinv <- function(B) invA <<- B
    
    ## Returns a list with the accessors/mutators required for cache
    ## functionality.
    list(set = set, get = get, getinv = getinv, setinv = setinv)

}


## Retrieves a cached inverse of a matrix, or if not available;
## calculates a new inverse, stores it in cache, and returns the inverse.

cacheSolve <- function(A, ...) {

    invA <- A$getinv()
    
    if(is.null(invA)) {
        invA <- solve(A$get(), ...)
        A$setinv(invA)
    } else {
        message('getting cached data')
    }
    
    ## Return a matrix that is the inverse of 'A'
    invA
    
}
