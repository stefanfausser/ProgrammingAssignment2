# Coursera R programming course
# Assignment 2
# Based on R. D. Pengs example with caching the mean of a vector

# Caching the inverse of a matrix

# 'makeCacheMatrix' creates a matrix object that can cache its inverse
# Input: x: matrix
# Output: list with functions: 
#         set(y): sets matrix 'x' to matrix 'y'
#         get: gets matrix 'x'
#         setinverse(inverse): sets inverse of 'x' to 'inverse'
#         getinverse: gets inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
    Ainv <- NULL # local object for matrix inverse
    set <- function(y) {
        x <<- y
        Ainv <<- NULL # clear the matrix inverse
    }
    get <- function() A
    setinverse <- function(inverse) Ainv <<- inverse
    getinverse <- function() Ainv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# 'cacheSolve' computes the inverse of the matrix object returned by 'makeCacheMatrix'
# Attention: It is assumed that the matrix is invertible
# Input: x: matrix
#        ...: additional arguments to 'solve'
# Output: Inverse of 'x'

cacheSolve <- function(x, ...) {
    Ainv <- x$getinverse()
    if(!is.null(Ainv)) {
        message("getting cached data")
        return(Ainv)
    }
    message("calculating inverse of matrix")
    data <- x$get()
    Ainv <- solve(data, ...)
    message("caching inverse of matrix")
    x$setinverse(Ainv)
    Ainv
}

# following is an example that uses the 'makeCacheMatrix' and 'cacheSolve' functions

# create an invertible 3x3 matrix A

A <- matrix(1,3,3)
diag(A) <- 2
print(A)

# create a cachable matrix object B from A

B <- makeCacheMatrix(A)

# calculate the inverse of B

print(cacheSolve(B))

# again, calculate the inverse of B (should be cached, see message)

print(cacheSolve(B))

# again, calculate the inverse of B (should be cached, see message)

print(cacheSolve(B))
