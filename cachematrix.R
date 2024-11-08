## These functions help avoid redundant matrix inversion calculations by caching the result for later use.
## makeCacheMatrix function creates a special object that stores a matrix and can cache its inverse.
## cacheSolve function computes the inverse of the matrix stored in the special object created by makeCacheMatrix.


## This function will create a special matrix object that can store both the matrix and its inverse. It uses closures to store data in its environment. It has four sub-functions:-
## 1.set:Sets the value of the matrix.
## 2.get:Retrieves the value of the matrix.
## 3.setInv:Caches the inverse of the matrix.
## 4.getInv:Retrieves the cached inverse of the matrix if it exists.

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y) {
    x<<-y
    inv<<-NULL
  }
  get=function() x
  setInv=function(inverse) inv<<-inverse
  getInv=function() inv
  list(set=set,get=get,setInverse=setInv,getInverse=getInv)
}


## This function will compute the inverse of the matrix created by makeCacheMatrix.
## If the inverse is already cached,it retrieves the cached value instead of recomputing it. Otherwise, it computes the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
    inv=x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
    m=x$get() 
    inv=solve(m,...) 
    x$setInverse(inv)
    inv
}
