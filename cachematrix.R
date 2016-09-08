## The following functions inverse of a matrix and store it in cache.


##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to
##
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverted matrix
##4.  get the value of the inverted matrix



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setreverse <- function(reversematrix) m <<- reversematrix
        getreverse <- function() m
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## The following function calculates the mean of the special "matrix"
## created with the above function. However, it first checks to see if the
## martix already inversed.If so, it `get`s inversed matrix from the
## cache and skips the computation. Otherwise, it inverse the matrix 
## and store it in cache via the `setreverse`function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getreverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setreverse(m)
        m
}