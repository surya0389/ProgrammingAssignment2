## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize the inverse property
        i <- NULL

        ## Method to set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }

        ## Method the get the matrix
        get <- function() {
                ## Return the matrix
                m
        }

        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }

        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse
                i
        }

        ## Return a list of the methods
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" created with
## the "makeCacheMatrix" function above. However, it first checks to see if the inverse
## has already been computed. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it computes the inverse of the "matrix" and sets the value of
## the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {

        ## get a matrix that is the inverse of 'x'
        i <- x$getInverse()

        ## Just return the inverse if its already exists
        if( !is.null(i) ) {
                message("getting data from cache")
                return(i)
        }

        ## Get the matrix from our object
        data <- x$get()

        ## Calculate the inverse using solve function
        i <- solve(data, ...) 

        ## Set the inverse to the object
        x$setInverse(i)

        ## Return the inverse
        i
} 

