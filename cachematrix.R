## Two functions to cache inverse of a matrix, so that no need to compute the inverse repeatedly.

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse, which
## is really a list containing function to set the value, get the value, set the inverse and get the
## inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, then cacheSolve should
## retrieve the inverse from the cache. Otherwise, it calculates the inverse of the data and sets the
## value of the inv in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
       i <- x$getinv()
       if(!is.null(i)) {
               message("getting cached data")
               return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setinv(i)
       i
}
