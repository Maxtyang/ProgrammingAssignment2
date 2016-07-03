##Function "makeCacheMatrix" creates a specal "matrix" object that can cache it's inverse. makeCacheMatrix includes 4 functions
        ##1. set
        ##2. get
        ##3. setmean 
        ##4. getmean

makeCasheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(solve) m <<- inverse
        getinverse<- function() m
        list(set = set, get = get, setinverse = setinverse, getinvserse = getinverse)
        
}


##Function "CacheSolve" computes the inverse of the special "matrix" which is returned by makeCacheMatrix. 

CacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(m)
        m
}