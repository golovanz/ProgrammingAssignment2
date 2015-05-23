## Make and use a special "CacheMatrix" as a list of functions 
## to cache and access the matrix inverse value

## Create a "CacheMatrix" object

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL ## the cached inverse matrix
  
  set <- function(y) {
      x <<- y
      ix <<- NULL ## clear the cache value
  }
  
  get <- function() x
  
  setinv <- function(minv) ix <<- minv
  getinv <- function() ix
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Calculate inverse matrix and cache it or return value from the cache

cacheSolve <- function(x, ...) {
                                
  mm <- x$getinv()
  if (!is.null(mm)) 
    return(mm) ## get cached inverse matrix
  
  data <- x$get() ## use matrix value given
  mm <- solve(data) ## calculate inverse matrix value
  x$setinv(mm) ## cache inverse mtrix value
  mm
}
