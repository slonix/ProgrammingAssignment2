## This function creates a special "matrix" object.
##
## It returns a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix if set


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(given_matrix) {
        x <<- given_matrix
        inverse <- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
