## cachematrix.R - A thoroughly original solution by J. Stam 2015-03-21
##
## The following functions create a matrix-like object that is a list of functions for manipulating 
## the encapsulated matrix.
##
## The inverse of a CacheMatrix object can then be computed and is simultaneously cached for a quick 
## lookup as opposed to recomputing every time the solver is called.


## makeCacheMatrix takes a matrix argument and returns a list-object of matrix manipulating function goodness

makeCacheMatrix <- function(x = matrix()) {
    
    #Initialize inverse of newly created matrix to NULL
    xinv <- NULL  
    
    #Call to directly set object to matrix y, inverse reset to null
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    #Return the actual matrix inside the object
    get <- function() x
    
    #Global assignment of supplied value to xinv, necessary for caching of inverse computation
    setInverse <- function(inverse) xinv <<- inverse
    
    #Return inverse of matrix 
    getInverse <- function() xinv
    
    #Return function list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve takes an object of the type created above and attempts to compute the inverse of the matrix.
## If the matrix has previously been computed, it will instead used the cached data. Additional arguments are
## passed to the solve function.

cacheSolve <- function(x, ...) {
    
    #If previously computed, if statement prints "cached" data, if not computed (i.e. null), inverse is calculated
    xinv <- x$getInverse()
    
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    
    data <- x$get()
    
    xinv <- solve(data, ...)
    
    x$setInverse(xinv)
    
    #return inverse of matrix
    xinv
}
