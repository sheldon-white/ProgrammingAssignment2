## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, debug = FALSE) {
  m <- x$getInverse()
  if(!is.null(m)) {
    if (debug) {
      message("returning cached data:")
      print(m)
      message()
    }
    return(m)
  }
  data <- x$get()
  if (debug) {
    message("calculating inverse of:")
    print(data)
    message()
  }
  m <- solve(data)
  if (debug) {
    message("caching calculated inverse:")
    print(m)
    message()
  }
  x$setInverse(m)
  m
}

c <- matrix(1:4, nrow=2, ncol=2)
matrixMaker = makeCacheMatrix()
matrixMaker$set(c)

inv1 <- cacheSolve(matrixMaker, debug=TRUE)
im1 = c %*% inv1
im1
inv2 <- cacheSolve(matrixMaker, debug=TRUE)
im2 <- c %*% inv2
im2
