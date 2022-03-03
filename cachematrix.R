## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(y = matrix()) {
        inv = NULL
        set = function(x) {
                y <<- x
                inv <<- NULL
        }
        get = function() y
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(y, u) {
        inv = y$getinv()
        if (!is.null(inv)){
                # if inv is not null
                return(inv)
        }
        mat.data = y$get()
        inv = solve(mat.data, u)
        y$setinv(inv)
        return(inv)
}
