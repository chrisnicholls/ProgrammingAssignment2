## This code computes and caches the inverse of a specified square matrix. A 
## matrix is first wrapped using the makeCacheMatrix function. That result can
## then be passed into cacheSolve to solve and cache the inverse. 
## 
## Example Usage:
## > m <- makeCacheMatrix(matrix(nrow = 2, ncol = 2, 1:4))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## returning cached inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## A wrapper object for a square matrix that can store its inverse as well as  
## its original matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #set a new matrix
    set <- function(y) {
        x <<- y
        #make sure to remove the cached inverse
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## A function that returns the inverse of the matrix. If the inverse has be 
## previously solved, then it will be returned from cache
## Takes as input a matrix wrapped by makeCacheMatrix

cacheSolve <- function(x, ...) {   
    
        #check if we have already computed the inverse and return it
        if(!is.null(x$getinverse())) {
            message("returning cached inverse")
            return(x$getinverse())
        }
        
        #compute the inverse
        i <- solve(x$get(), ...)
        #cache it
        x$setinverse(i)
        i
}
