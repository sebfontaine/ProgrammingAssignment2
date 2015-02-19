## This file contains the source code for 2 functions:
## - makeCacheMatrix creates a special matrix object that can store its inverse
## - cacheSolve returns the inverse of a matrix created with makeCacheMatrix by
## either calculating it or returning the cached value.

## This function creates a special kind of matrix (actually a list) which
## has 4 functions:
## - get() : returns the 'regular' R matrix stored in the list
## - set() : changes the 'regular' R matrix stored in the list
## - get_inverse() : returns the inverse of the matrix
## - set_inverse() : stores the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # initialize the inverse to NULL value (not known yet).
    
    ## the next function can be used to assign a value to the matrix if the.
    ## makeCacheMatrix function was called with no argument.
    ## Argument y needs to be a matrix.
    ## EXAMPLE :
    ## > m<-makeCacheMatrix()
    ## > m$set(matrix(1:4,nrow=2,ncol=2))
    set <- function(y) {
        x <<- y # x is now the matrix passed as an argument to the set function.
        inverse <<- NULL # initialize the inverse (not known yet).
    }
    get <- function() x # this function merely returns the 'regular' R matrix.
    set_inverse <- function(i) inverse <<- i # this function stores the inverse
    # of the matrix; argument i is a matrix which will be the cached value of
    # the inverse.
    get_inverse <- function() inverse # returns the value of the inverse stored
    # in the list.
    
    # This list is what the function returns. Its elements are functions which
    # enable one to:
    # - assign a new value to the CacheMatrix (set)
    # - return the current value of the CacheMatrix (get)
    # - assign a value to the inverse of the CacheMatrix. This will be the 
    # cached inverse of the matrix (set_inverse)
    # - return the cached value for the inverse of the matrix (get_inverse).
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function returns the inverse of the CacheMatrix passed as its argument
## and stores it in the list.

cacheSolve <- function(x, ...) {
    # Check the contents of the cache in the CacheMatrix
    inverse <- x$get_inverse()
    
    # if there is already a cached value, return it without any calculation.
    if (!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    
    matrix <- x$get()
    
    ## Calculate the inverse, cache it and return it
    inverse <- solve(matrix)
    x$set_inverse(inverse) # stores the calculated inverse
    inverse # return the matrix invert
}
