## These functions implement the programming assignment:
## (https://class.coursera.org/rprog-004/human_grading/view/courses/972139/assessments/3/submissions)
## Please refer to that page for a full description of the assignment.

# Implement a simple cache. This is creating a closure, since the functions like set() and get()
# operate on the environment variables ('x' and 'm') defined inside makeCacheMatrix()
#
# Args:
#   x: The matrix to be cached
#
# Returns:
#   A list of functions that can be called by name. It's similar to a virtual-method table in C++.
makeCacheMatrix <- function(x = matrix()) {
    # initalize cached value
    m <- NULL
    
    # setter function, which also clears the cached result (if any)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # getter function
    get <- function() {
        x
    }
    
    # cache the calculated result
    setInverse <- function(inverse) {
        m <<- inverse
    }
    
    
    # retrieve the calculated result
    getInverse <- function() {
        m
    }
    
    # return a handle to the function table (essentially)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Calculate the inverse (if any) of a supplied matrix. This is a barebones implementation that lacks
# several things:
# 1) It should check that a square matrix is being passed in.
# 2) It should have nicer error handling in the matrix is not invertable
#    (see http://en.wikipedia.org/wiki/Invertible_matrix)
# This uses a matrix cache as a data container and to allow the return of a cached value when possible.
# In real life I might use the 'memoise' package
#    (http://cran.r-project.org/web/packages/memoise/index.html)
#
# Args:
#   x: a function list returned from a makeCacheMatrix() call.
#   verbose: If TRUE, prints messages describing the internal activity. Default is FALSE
#
# Returns:
#   The inverse matrix if it exists. If the matrix isn't invertable an error will be thrown.
cacheSolve <- function(x, verbose = FALSE) {
    m <- x$getInverse()
    if (!is.null(m)) {
        if (verbose) {
            message("returning cached data:")
            print(m)
            message()
        }
        return(m)
    }
    data <- x$get()
    if (verbose) {
        message("calculating inverse of:")
        print(data)
        message()
    }
    m <- solve(data)
    if (verbose) {
        message("caching calculated inverse:")
        print(m)
        message()
    }
    x$setInverse(m)
    m
}
