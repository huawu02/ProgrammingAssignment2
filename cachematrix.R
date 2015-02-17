## example script to cache the inverse of matrics by using makeCacheMatrix 
## and cachSolve:

## > source('cachematrix.R')
## > a <- matrix(c(1,2,3,5), 2, 2)
## > ma <- makeCacheMatrix(a)
## > cacheSolve(ma)
##      [,1] [,2]
## [1,]   -5    3
## [2,]    2   -1
## > cacheSolve(ma)
## getting cached data
##      [,1] [,2]
## [1,]   -5    3
## [2,]    2   -1


## create a matrix with its inversion saved if it exists
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

## solve the inversion of a matrix, search for the already existing inversion first
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setinv(inv)
    inv 
}
