##These functions are used to calculate the inverse of a matrix and then storing
##the result to cache so that the result can be retrieved again later


##makeCacheMatrix function creates a special "vector", which is a list containing a 
##function to set the value of the matrix, get the value of the matrix, set the 
##value of the inverse matrix and the get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##cacheSolve function calculates the inverse of the matrix using the output
##produced from the makeCacheMatrix function. However. it first checks to see
##if the inverse has already been calculated. If so, it gets the result from
##the cache and skips the computation. Otherwise, it calculates the inverse
##matrix and set the result to cache via the setinv function 

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
