## These functions obtain the inverse of a matrix.
## When it is computed, the inverse of the matrix is cached.
## If it is already cached, the inverse is returned and the calculation is skipped. 
## In this functions, it is assumed that the matrix supplied is always invertible.


## This function creates a special "matrix" object, which returns a list containing a function to
## set and get the value of the matrix, and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        # The operator <<- causes a search for the variables x and i through parent environments
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    #if i is not found through parent environments, assignment takes place in the global environment
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
## The x parameter is a list of functions like the list returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    
    if (!is.list(x)){
        stop("x must be a list of functions")
    }
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    
    #solve computes the inverse of a matrix
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
