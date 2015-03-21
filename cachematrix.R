## The following functions are developed to perform the following:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  
## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "vector" of type Matrix, 
## which is really a list containing a function to
##     set: set the value of the vector
##     get: get the value of the vector
##     setinverse: set the value of the inverse
##     getinverse: get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
         set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 }

## The following function calculates the inverse of the special "vector" of type matrix 
## created with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Else, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinverse()
         if(!is.null(i)) {
                 message("getting cached data")
                 return(i)
         }
         data <- x$get()
         i <- solve(data, ...)
         x$setinverse(i)
         i        
}
