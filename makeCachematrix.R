##Function for cache matix
makeCachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  ## inversing the matrix
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
##getting inverse matrix as well as cached data
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data..!!")
    return(inv)
  }
  ##setting the Inverse matrix in the setInverse
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
}
