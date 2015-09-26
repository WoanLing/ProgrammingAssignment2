## Caching the inverse of a matrix
## makeCacheMatrix : creates a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
    z <-NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse)z <<-inverse
    getInverse <- function() z
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve : computes the inverse of the special "matrix" returned by the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(!is.null(z)){
            message("getting cached data")
            return(z)
        }
        data <- x$get()
        z<-solve(data,...)
        x$setInverse(z)
        z
}
