## The following pair of functions can cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is a list containing a function to
##      1. set the the value of the matrix, 
##      2. get the value of the matrix, 
##      3. set the value of the inverse of the matrix, 
##      4. get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## The second function, cacheSolve caculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("get cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
