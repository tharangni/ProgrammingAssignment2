## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(b) {
                x <<- b
                i <<- NULL
        }
        
        getMatrix <- function() x
        
        setInverse <- function(i) i <<- solve(x)
        
        getInverse <- function() i
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        if((!is.null(i))) {
                message("getting cached inverse")
                return(i)
        }
        
        message("computing new inverse")
        unknown <- x$getMatrix()
        i <- x$setInverse(unknown)
        i
}
