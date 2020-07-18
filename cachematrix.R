## JHU Coursera Data Science Specialization Course 2: R Programming
## Assignment 2: Lexical Scoping in R
## Author: Saad Siddiqui
## cacheMatrix.R: A pair of functions that demonstrate the principles of lexical
## scoping in R by caching the inverse of a matrix.

## Creates a Matrix object with getters and setters for matrix elements and 
## a cached matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initially, inverse is undefined
    mat_inv <- NULL
    
    # Sets the values of a matrix and resets its mat_inv attribute
    set <- function(new_matrix) {
        # Reinitialise the formal variable `x`
        x <<- new_matrix 
        
        # Also reset the inverse attribute to NULL
        mat_inv <<- NULL
    }
    
    # Returns the cachedMatrix
    get <- function(x)
    
    # Setter for the cached matrix inverse
    setInverse <- function(inv) mat_inv <<- inv
    
    # Getter for the cached matrix inverse 
    getInverse <- function() mat_inv
    
    # Return the matrix object as a `list` with the matrix itself
    # and all related attributes and methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function that initialises and tests a `cacheMatrix`
cacheSolve <- function(x, ...) {
    # First, try and get the cached value of the matrix's inverse
    inv <- x$getInverse() 
    
    # Echo cached value and a message identifying it only if cached value defined
    if (!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    
    # If cached inverse is undefined, compute and set 
    mat <- x$get()          # Get the values of the matrix
    inv <- solve(mat,...)   # Compute its inverse
    x$setInverse(inv)       # Update the cached inverse
    inv                     # Return the inverse
}