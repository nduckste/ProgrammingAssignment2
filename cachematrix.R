
#The functions in this file server to create a matrix.

#Once created, the inner functions can solve the matrix (creating the inverse)
# and caching it.

#Upon subsequent requests for the inverse of the original matrix,
# the inner functions get the inverse from the cache, thus saving 
# a recompute of the inverse matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}



#Return a matrix that is the inverse of 'x'
#This function retrieves the cached inverse matrix if it exists.
#If it doesn't it exist, it creates the inverse and caches it.
cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}