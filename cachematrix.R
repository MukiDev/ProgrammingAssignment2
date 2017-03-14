## The functions below allow calculating the inverse of a matrix taking advantage of
## lexical scoping in R to cache a previously calculated value, avoiding costly re-calculations 

## makeCacheMatrix : This function uses a matrix object as input and creates a list object as 
## output. The elements of the list are the functions to set and retrieve both cached values
## x and i ( matrix and inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(z) {i <<- z}
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve : This function uses a list argument, created using the makeCacheMatrix function,
## to retrieve (if existent) or calculate and store (if not) the cached valued of the inverse of
## a matrix. It also displays such value as an output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        message("calculating inverse")
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}






