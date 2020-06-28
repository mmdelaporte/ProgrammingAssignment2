## The following functions will allow to save in cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y = matrix()) {
                x <<-y
                inv<<-NULL       
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
       inv<-x$getinverse()
       if (!is.null(inv)) {
               message("getting cached data")
               return(inv)
       }
       data <- x$get()
       inv <- solve(data)
       x$setinverse(inv)
       inv     ## Return a matrix that is the inverse of 'x'
}

