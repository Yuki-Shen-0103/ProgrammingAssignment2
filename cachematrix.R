## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix create a matrix,which is a list containing four functions 
## to set/get the value of matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function cacheSolve calculate the inverse of the matrix created with the above function
## after check whether the inverse has already been calucated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
