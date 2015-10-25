## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x<<-y
            inv<<-NULL
      }
      get <- function() x
      setSolve <- function(solve) inv<<- solve
      getSolve <- function() inv
      list(set=set,get=get,
           setSolve = setSolve,
           getSolve = getSolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getSolve()
        if(!is.null(inv)){
                message("Getting cached data..")
                return(inv)
        }
        data <-x$get()
        inv <- solve(data,...)
        x$setSolve(inv)
        inv
}
