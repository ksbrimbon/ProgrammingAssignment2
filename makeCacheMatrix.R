#create a function that is for caching the mean of the vetor
#in this case matrix is the vector
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL      #inverse value printed as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL   #the inverse is set to NULL
  }
  get <- function (){x}   #function to get matrix x
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){     ##get the cache data
  inv <- x$getInverse()
  if(!is.null(inv)){      #to check if the inverse value is null
    message("getting cached data")   #message prompt that will appear
    return(inv)         #returns the inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)    #to calculate the inverse value
  x$setInverse(inv)
  inv
}