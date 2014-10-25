##This function creates a matrix which stores the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    set  <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) inv  <<- inverse
    getinverse  <- function() inv
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

## This function either computes the inverse matriv or returns the cached
##inverse matrix if present. 


cacheSolve <- function(x, ...) {
    inv  <- x$getinverse()
    if (!is.null(inv)){ #Checks if the inverse matrix was cached or not
        message("getting cached data")
        return(inv)
    }
    data  <- x$get()
    inv  <- solve(data, ...) #calculates the inverse matrix
    x$setinverse(inv)
    inv
}