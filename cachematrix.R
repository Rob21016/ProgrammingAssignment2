# To test this functions I used the matrix provided by mentor Alan Berger 
# on the forum. He specifically says that it's OK to use that matrix.

# This function has the matrix mx as the argument
makeCacheMatrix <- function(mx) {
    i <- NULL            # i is the inverse matrix of mx and is initialiazed as null
    set <- function(y) { # the set function may be used to change the matrix without re-initializing makeCacheMatrix
        mx <<- y         # this assigns the value of y to mx in the parent environment
        i <<- NULL       # this assigns null to mx in the parent environment
    }
    get <- function() mx                            # this gets mx from the parent environment
    setinverse <- function(inverse) i <<- inverse   # this assigns the value inverse to i in the 
    # parent environment
    getinverse <- function() i                      # this gets the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)                   # this returns a list of named functions
}

# this function checks if the inverse matrix already exists. If not, it calculates it.
cacheSolve <- function(mx, ...) {
    i <- mx$getinverse()           # it gets the inverse matrix from makeCacheMatrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)                  # this checks if i (the inverse matrix) is null or not. If not, it 
        # it return the cached i
    }
    data <- mx$get()               # if i is null, then it gets mx from makeCacheMatrix
    i <- solve(data)               # it calculates the inverse of mx and assigns it to i
    mx$setinverse(i)               # in the setinverse function within makeCacheMatrix the inverse matrix 
    # is assigned to i in the parent environment
    i
}

# I used the following lines to check that the functions worked correctly
# mx = matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# matinv <- makeCacheMatrix(mx)
# cacheSolve(matinv)