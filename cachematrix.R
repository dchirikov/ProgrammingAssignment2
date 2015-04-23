## makeCacheMatrix get matrix as an input and returns
## object which has 4 functions:
## set - to set matix x inside object
## get - get matrix
## setinverse - set inversed matrix
## getinverse - get inversed matrix
## functions do

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function() {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverseMatrix) {
        i <<- inverseMatrix
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Operates with an object returned by makeCacheMatrix function.
## If getinverse function has been already invoked it returns inverse matrix.
## If no - executes solve() function and executes setinverse procedure
## in object returned by makeCacheMatrix function to set i value in it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
