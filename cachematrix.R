## Put comments here that give an overall description of what your
## functions do
This Function overall is showing how to cache the inverse of a matrix
Creates a "matrix" contains (set and get of matrix and set and get of inverse matrix)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        MyInverse <- Null
        
        Set <- function(y)
                x <<- y
        Myinverse <<- NULL
}

        Get <- function() x
        setinverse <- function(solve) MyInverse <<- solve
        Getinverse <- Function() Myinverse
        list (set = set, get = get, SetInverse = SetInverse, 
              Get Inverse = GetInverse)
}




## Write a short comment describing this function
Computes the CacheMatric to get an invrese. Does not change function Cachesolve
but gets inverse from the cache. However if it does not get it fom Cache it
it calculates it from matrix and sets inverse matrix in the cache through
the setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        MyInverse <- x$GetInverse()
        if(!is.null(MyInverse)) {
                message ("Getting Cached Data")
                return(Myinverse)
        }
        
        MyData <- x$get()
        MyInverse <- solve(MyData, ...)
        x$SetInverse(MyInverse)
        MyInverse
        }
}
