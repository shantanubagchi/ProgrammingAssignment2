
## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve would retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatinv <- function(solve) s <<- solve
        getmatinv <- function() s
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}

##  The function cacheSolve calculates the inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setmatinv function.

cacheSolve <- function(x, ...) {
        s <- x$getmatinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setmatinv(s)
        s
}

