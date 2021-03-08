## Create a function that creates a special "matrix" object that can
## cache its inverse and another function that computes the inverse of 
## the special "matrix". If the inverse has already been calculated then
## it should retrieve the inverse from the cache.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    m <-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list (set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
}


## Compute the inverse of the special "matrix". If the inverse
## has already been calculated then it should retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
    
