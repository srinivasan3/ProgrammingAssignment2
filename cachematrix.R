##  functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y){
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinv <- function(inv) c <<- inv
        getinv <- function() c
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getinv()
        
        if (!is.null(c)){
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data,...)
        x$setinv(c)
        c
}
