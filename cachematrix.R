## Put comments here that give an overall description of what your
## functions do

## This Function overall is showing how to compute and cache the inverse of a matrix.
## Overall this is supposed to be able to reduce computation time. This is done by 
## by creating objects, a list of four functions, that are used to determine if the cache has already been
## computed. If the computation is found then the result is looked up and returned
## However if not then the inverse would be looked up and returned.

## Write a short comment describing this function
## The point of this function first shows that the cachematrix is creating another Matrix which contains the list of four
## functions that were mentioned earlier. These functions are 1) Set the value of matrix 2) get the value of the 
## matrix 3) set the value of the matrix 4) get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {

        Inverse_x <- Null
        
        Set <- function(y)
                x <<- y
        Inverse_x <<- NULL
}

        get <- function() x
        setinverse <- function(inverse) Inverse_x <<- inverse
        Getinverse <- Function() Inverse_x
        list (set = set, get = get, SetInverse = SetInverse, 
              Get Inverse = GetInverse)
}

## Write a short comment describing this function
## Cachesolve is used to check if the inverse of the given matrix has already been computed.
## Again if already computed it will return the given inverse, a cached version ( used to increase speed/efficiency). 
## If not then it will calculate and return an inverse

cacheSolve <- function(x, ...) {
        ## Return a matric that is the inverse of 'x'
        ## If inverse is already computed then return it

        Inverse_x <- x$GetInverse()
        if(!is.null(MyInverse)) {
                message ("Getting Cached inverse of matrix")
                return(Inverse_x) # returns cache
        }
        data <- x$get() # placed here instead of further below because we need to create vector for determinent 
        
        ## important to note is to see if data given is a singular request
        determinent <- det(data)
        if (determinent == 0){
                message("Matrix is singular and cannot be inverted")
                return(invisible())
        }
        ## So if data is not singular then you would compute the inverse
        
        Inverse_x <- solve(data, ...)
        x$SetInverse(Inverse_x) # saves in cache
        Inverse_x
        }
}

