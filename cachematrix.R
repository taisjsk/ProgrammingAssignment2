## This function will take the inverse of a matrix input and cache it 
## This script was written for Assignment #2 on R Programming 

#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set,get = get,
             setinv = setinv,
             getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from the
# cache

cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix.data <- x$get()
        inv <- solve(matrix.data, ...)
        x$setinv(inv)
        return(inv)
}