makeCacheMatrix <- function(x = matrix()) {
  nu1<-NULL
  set<-function(fun1){
    x<<-fun1
    n1<<-NULL
  }
  get<-function()x
  setinv<-function(solve) nu1<<-solve
  getinv<-function()nu1
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  n1<-x$getinv()
  if (!is.null(n1)){
    message("getting cached data")
    return (n1)
  }
  data<-x$get()
  n1<-solve(data)
  x$setinv(n1)
  n1
}
