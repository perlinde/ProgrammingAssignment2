## The functions can be used to cache the inverse of a matrix.
## makeCacheMatrix takes a matrix and makes a list with a function to
## set and get the value of the matrix, as well as the inverse of the matrix.
## cacheSolve returns the inverse of the matrix.
## cacheSolve assumes an invertible matrix.

## Create list containing function to set & get value of matrix
## and inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## Checks if inverse has been computed. If yes, it gets the result.
## If not, computes inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    ## Returns a matrix that is the inverse of 'x'
     data <- x$get()
     i <- solve(data)
    x$setinverse(i)
    i
}
