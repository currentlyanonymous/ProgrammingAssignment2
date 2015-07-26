makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set_matrix <- function(a){
    x<<-a
    m <<- NULL
  }
  
  get_matrix <- function() x
  
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  
  list(set = set_matrix, get = get_matrix, 
       setinv = set_inverse, getinv = get_inverse)
}

cacheSolve <- function(x,...){
  m <- x$getinv()
  if (!is.null(m)){
    return(m)
  }
  
  d <- x$get()
  m <- solve(d,...)
  x$setinv(m)
  m
  
}
