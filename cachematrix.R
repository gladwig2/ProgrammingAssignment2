## Week3 Assignment2, creates a cacheable matrix 
##

## makeCacheMatrix:This function creates a special "matrix" object that can 
#  cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
 }

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve retrieves the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}



#> m3x3 <- matrix(c(1,2,3,4,5,6,7,8,10),3,3)
#> mc = makeCacheMatrix(m3x3)
#> cacheSolve(mc)
#[,1]       [,2] [,3]
#[1,] -0.6666667 -0.6666667    1
#[2,] -1.3333333  3.6666667   -2
#[3,]  1.0000000 -2.0000000    1
#> cacheSolve(mc)
#getting cached data
#[,1]       [,2] [,3]
#[1,] -0.6666667 -0.6666667    1
#[2,] -1.3333333  3.6666667   -2
#[3,]  1.0000000 -2.0000000    1
#> 