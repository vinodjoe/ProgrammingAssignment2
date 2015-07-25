## Cache a matrix 
## a function containg the 4 funtctions 

makeCacheMatrix <- function(x = matrix()) {
    g.inv<-NULL
    set<- function(y){
        x <<- y
        inv<<-NULL
    }
    get <- function () x
    setinv<- function(inverse) g.inv<<- inverse
    getinv<- function() g.inv
    list(set = set,get =get,setinv = setinv,getinv = getinv)
}



## function to produce the inverse as well as check if it the data is in the 
## cache

cacheSolve <- function(x=matrix(), ...) {
      inv <-x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv 
}
