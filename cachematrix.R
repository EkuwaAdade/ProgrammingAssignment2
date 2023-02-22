## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## mkeCacheMatrix includes ser,get,setinv,getinv
library(MASS)

makeCacheMatrix <- function(m = matrix()) {
inv <- NULL    #this function initializes inverse as NULL
set <- function(n){
  m <<- n
  inv <<- NULL
}
get <- function()m  #this function is for getting the matrix m
setinv <- function(inverse)inv<<-inverse
getinv <- function(){
  inver <- ginv(m)
  inver%%m      #this function obtains inverse of a matrix
}
list(set=set,get=get,
     setinv = setinv,
     getinv = getinv)
}


## Write a short comment describing this function
## for cache data
cacheSolve <- function(m, ...)##gets cached data
  {
  inv <- m$getinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data <- m4get()
  inv <- solve(data,...)
  m$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'm'
}
