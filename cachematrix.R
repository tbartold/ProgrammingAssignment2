### The two functions in this file cache a matrix, compute its inverse and cache that also 

# makeCacheMatrix will cache a matrix and provides structure to hold the inverse of the matrix
# the input should be a valid matrix - create the matrix and assign it to a variable
# e.g.: my_matrix = makeCacheMatrix(matrix(c(1,2,2,3),2,2))

makeCacheMatrix <- function(x = matrix()) {
    ## Stores a matrix and creates a placeholder for its inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# cacheSolve will compute the inverse of the matrix and cache the result
# the input should be a matrix made by the makeCacheMatrix function above
# e.g. cacheSolve(my_matrix) will return the inverse and cache it

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
