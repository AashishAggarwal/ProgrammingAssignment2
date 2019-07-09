## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a special "matrix" object that will cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    mtrx <- NULL
    set <- function(y) {
        x <<- y
        mtrx <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mtrx <<- solve
    getinv <- function() mtrx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## Will retrieve the cache inverse of the matrix if already calculated
## by the above function else will calculate the inverse here.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mtrx <- x$getinv()
    if(!is.null(mtrx)) {
        message("getting cached data")
        return(mtrx)
    }
    data <- x$get()
    mtrx <- solve(data, ...)
    x$setinv(mtrx)
    mtrx
}
