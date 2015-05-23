## The first function, makeCacheMatrix creates a special "vector", 
## 

## set the value of the vector
## get the value of the vector
## set the value of the solve function
## get the value of the solve

## 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setCache <- function(solve) m <<- solve
    getCache <- function() m
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
}


## calculates the inverse of the special "vector" created with the 
## makeCacheMatrix above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setCache function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getCache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setCache(m)
    m
}
