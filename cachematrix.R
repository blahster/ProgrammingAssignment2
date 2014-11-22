## Memoized matrix inversion
## Use case - the inverse of a (unchanging) matrix is required several times.
## Instead of computing it each time, we cache the inverse and return it.
## Assumptions:
##   matrix is invertible
##   matrix does not change

## Usage:
## 1. Call makeCacheMatrix with a matrix; returns a (list) object with accessors
## Ex: m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2)) # 2x2 matrix
## 2. Call cacheSolve on the return of the object returned from makeCacheMatrix
## cacheSolve(m1) # computes inverse and returns it
## cacheSolve(m1) # returns cached inverse with message "getting cached inverse"

## Create a special "matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## If inverse present, return it. Otherwise, compute and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
