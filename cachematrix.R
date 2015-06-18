## Put comments here that give an overall description of what your
## functions do

## create a cached Matrix object - if a function cacheSolve is called on this object repeatedly,
## it'll only compute the inverse of the matrix on the first call.  Subsequent calls will use the 
## cached memory inversion solution

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function is called on the previously created cached Matrix object.  It's the same functionality
## as the solve function on a matrix, except it returns the cached solution, if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
