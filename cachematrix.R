## the following two functions work together to allow the user of a matrix to cache the inverse
## of a matrix; potentially saving computation time.

## makeCacheMatrix wraps a matrix with behavior that allows users to set and fetch the inverse of a matrix
## helper methods to get and set the underlying matrix data are also provided.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) i <<- solution
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a matrix that's been decorated by makeCacheMatrix 
## and implements a simple caching mechanism for the matrix inverse operation

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        } else {
            matrix <- x$get()
            i <- solve(matrix, ...)
            x$setinverse(i)
            return (i)
        }
}
