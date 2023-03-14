## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x  ##function to get matrix
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x   ##function to get inverse of matrix
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##This function is used to get the cached data

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){	##checking if inverse is null
    message("Getting cached data!")
    return(inv)		##returns inverse value
  }
  data<-x$get()
  inv<-solve(data, ...) ##calculates inverse value
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
