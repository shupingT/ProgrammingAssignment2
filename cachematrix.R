## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
  x<<-y
  a<<-NULL
}
get<-function() x
setmatrix<-function(solve) a<<- solve
getmatrix<-function() a
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}
#cacheSolve: This function computes the inverse of the special
#"matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
    a<-x$getmatrix()
    if(!is.null(a)){
            return(a)
    }
    matrix<-x$get()
    a<-solve(matrix, ...)
    x$setmatrix(a)
    a
}
# test the function 
b <- matrix(1:4, 2, 2)
b
test2<-makeCacheMatrix(b)
cacheSolve(test2)

