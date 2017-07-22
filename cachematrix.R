####     makeCacheMatrix & cacheSolve   ####
# makeCacheMatrix creates a function that returns a list of user defined properties,
# that are prescribed as functions, to a matrix which is the formal argument.
# The objects x and i store the matrix and it's inverse respectively. The role of
# the functions are described as follows:
# 1. setMatrix: sets the value of the matrix
# 2. getMatrix: gets/displays the matrix
# 3. setInverse: calculates the inverse of the matrix which is passed as an argument. 
#                 calculation is done using solve()
# 4. getInverse: gets/displays the inverse matrix


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

# cacheSolve() basically computes the inverse of the matrix that is passed as an argment.
# If the matrix that is passed to the cacheSolve, already has the inverse computed 
# previously using makeCacheMatrix(), then the inverse is just retrieved from that function.
# This process saves a lot of compuational time as it only retrives an existing value
# insted of computing it again.
# Otherwise if the matrix passed doesn't have it's inverse computed, then inverse 
# is computed for the passed matrix using setInverse function.

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
