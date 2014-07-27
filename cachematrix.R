## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse
## 

## This function creates a special "matrix" object that can cache its inverse
## returned object contains following 4 functions:
## 1 set(x): Assign real matrix x to this special object.
##           Assign NULL to inverse as the matrix changed. 
##           After this, user will have to re-cache the inverse.
##           If parameter is not Matrix, we won't do any change.
## 2 get(): return the real matrix.
## 3 setinverse(x): used to store inverse. Calculation should be done by cacheSolve function.
## 4 getinverse(): return the inverse of the real matrix.
## Only matrix is allowed to be saved.

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x))
        {
                message("Please give me a matrix")
                return()
        }
        inv <- NULL
        set <- function(y) {
                if(!is.matrix(y))
                        message("Please give me a matrix")
                else
                {
                        x <<- y
                        inv <<- NULL  
                }                        
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then retrieve the inverse from the cache.
## If the inverse has not been calculated, then compute the inverse and cache it.
## we assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'    
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}