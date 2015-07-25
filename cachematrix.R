## These functions invert a matrix and will cache the result
## so that future inversions of the same matrix will return
## a cached result instead of recalculating

## This function takes as input a matrix, and returns a list of functions
## and references to access a cached matrix inversion
##  get() - returns the matrix
##  set() - initializes the matrix object
##  setmatrixinv() - sets the matrix inversion object pointer
##  getmatrixinv() - returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setmatrixinv <- function (matrixInv) { 
        mInv <<- matrixInv
    }
    getmatrixinv <- function () mInv
    list(set = set, get = get,
         setmatrixinv = setmatrixinv, getmatrixinv = getmatrixinv)
}


## This function takes a matrix list containing references to a cached
## matrix object and returns an inverse matrix. If the result has already
## been computed it uses a cached version instead of recomputing

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getmatrixinv()
    if(!is.null(mInv)) {
        message ("getting cached maxtrix inverse")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data)
    x$setmatrixinv(mInv)
    return(mInv)
}
