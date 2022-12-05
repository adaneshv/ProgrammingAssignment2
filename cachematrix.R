## Put comments here that give an overall description of what your
## functions do

## The function, makeCacheMatrix creates a special "matrix" that can cache its inverse, which is really a list containing a function to:

  ## set the value of the matrix

  ## get the value of the matrix

  ## set the value of the inverse

  ## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## The "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x'
Matrix1<-matrix(c(3,2,6,8),2,2)
Test<-makeCacheMatrix(Matrix1)
cacheSolve(Test)

