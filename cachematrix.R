## Computing the inverse of a square matrix and caches it
## usage:
## a <- matrix(c(2, 2, 3, 2), ncol=2, nrow=2) ## creating square matrix
## c <- makeCacheMatrix(c) ## creating matrix proxy
## cacheSolve(c) ## calculating matrix inverse value (or get it from cache)

## Creates a matrix proxy that stores initial matrix and its inverse value

makeCacheMatrix <- function(x = matrix()) {
    invertedX <- NULL
    set <- function(y) {
        x <<- y
        invertedX <- NULL
    }
    get <- function() x
    setInvertedX <- function(invX) invertedX <<- invX
    getInvertedX <- function() invertedX
    list(set = set, get = get, setInvertedX = setInvertedX, getInvertedX = getInvertedX)
}


## Calculates and caches matrix inverse value or get it from cache if it calculated

cacheSolve <- function(x, ...) {
    invertedX <- x$getInvertedX()
    if (!is.null(invertedX)) {
        return(invertedX)
    }
    data <- x$get()
    invertedX <- solve(data, ...)
    x$setInvertedX(invertedX)
    invertedX
}
