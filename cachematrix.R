## Put comments here that give an overall description of what your
## functions do

## Wrapper for a matrix and it's inverse
## set(y), set the wrapped matrix to y
## get(), returns the wrapped matrix
## getinv(), returns the cached inverse of wrapped matrix
## setinv(matrixInverse), set the cached matrix inverse

makeCacheMatrix <- function(xMatrix = matrix()) {
    mInv <- NULL
    set <- function(y) {
        xMatrix <<- y
        mInv <<- NULL
    }
    get <- function() xMatrix
    setinv <- function(matrixInverse) mInv <<- matrixInverse
    getinv <- function() mInv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of a matrix contained in a makeCacheMatrix object 
## and caches it.
## If the inverse was already calculated, returned the cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getinv()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setinv(mInv)
    mInv
}
