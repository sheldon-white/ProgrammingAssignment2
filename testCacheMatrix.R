source("cachematrix.R")

c1 = matrix(1:4, nrow = 2, ncol=2)
mm1 = makeCacheMatrix()
mm1$set(c1)

# first run calculates the inverse
inv1 = cacheSolve(mm1, verbose=TRUE)
im1 = c1 %*% inv1
im1

# second run uses the cache
inv2 = cacheSolve(mm1, verbose=TRUE)
im2 = c1 %*% inv2
im2

# ...and we got back the same matrix each time
# this will return TRUE
identical(inv1, inv2)

# we can have multiple independent caches at the same time, since they use different environments
c2 = matrix(2:5, nrow = 2, ncol=2)
mm2 = makeCacheMatrix()
mm2$set(c2)

# this will return FALSE
identical(cacheSolve(mm1), cacheSolve(mm2))

# and we can reuse the cache
mm1$set(c2)
# now this will return TRUE
identical(cacheSolve(mm1), cacheSolve(mm2))

# if the matrix can't be inverted...
c3 = matrix(1:9, nrow = 3, ncol=3)
mm3 = makeCacheMatrix()
mm3$set(c3)
# this throws an error and inv3 is not assigned to anything
inv3 = cacheSolve(mm3)

