## Functions to calculate, store and retrieve an inverse matrix solution
## Created by David Alexander for the R Programming Week 3 Assignment.

## makeCacheMatrix creates storate to calculate the inverse of a matrix
## and stores it as m for later retrieval outside of the function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverseMatrix <- function(solve) m <<- solve
    get_inverseMatrix <- function() m
    list(set = set, get = get,
         set_inverseMatrix = set_inverseMatrix,
         get_inverseMatrix = get_inverseMatrix)
}


## cacheSolve uses the solve function to find the inverse of a
## matrix (square only).  If the function has already been run
## and the solution stored in cache, the cached value is returned.
## Otherwise, the solution is calculated and stored for future retrieval.

cacheSolve <- function(x, ...) {
    m <- x$get_inverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix.data <- x$get()
    m <- solve(matrix.data, ...)
    x$set_inverseMatrix(m)
    return(m)
}
