library(matlib)
makeCacheMatrix <- function(mat){
        m <- NULL
        set <- function(y) {
                mat <<- y
                m <<- NULL
        }
        get <- function() mat
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

cacheSolve <- function (mat,...){
        #print("here")
        m <- mat$getinv()
        #print("here2")
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #print("here3")
        data <- mat$get()
        #print("here4")
        m <- inv(data, ...)
        mat$setinv(m)
        m
}
