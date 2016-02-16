## this file giving a pair of function that can calculate inverse of a matrix and 
## cache the result for next time the inverse result is needed.

## makeCacheMatrix function that generate a matrix that can store cached inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve function get the inverse of a matrix. if it's 
## not called for the first time , it will return the cached result

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
