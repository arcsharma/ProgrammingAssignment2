## cacheSolve function calculates the inverse of a matrix. It first checks if the inverse value is already calculated or not. If inverse already available, it returns the value from cache without calculating else it calculates it.
## makeCacheMatrix function sets the value of x(matrix) and its inverse in the environment variable.

## This method creates a matrix and returns setter and getter for x and its inverse

makeCacheMatrix <- function(x = matrix()) {
	cached_Inverse_Matrix <- NULL  
	##set x in parent environment
	set <- function(value = matrix()) {
		x <<- value
		##Clear cache if its already set
		cached_Inverse_Matrix <<- NULL
	}  
	get <- function() x  
	
	setInverse <- function(inverseMatrix) {
	##set inverse value in parent environment
		cached_Inverse_Matrix <<- inverseMatrix
		return(cached_Inverse_Matrix)
	}
  
	getInverse <- function() cached_Inverse_Matrix
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This method returns inverse of a matrix. First it checks if the inverse is cached or not. If yes, it returns the cached value without calculating inverse else it calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
	inverse_matrix <- x$getInverse()
	##Checking if the inverse is cached or not
	if(!is.null(inverse_matrix)) {
		message("Inverse value is already calculated and cached. Returning value from cache!!")
		return(inverse_matrix)
	}else{
	##cached matrix not found so calculating the inverse.
		original_matrix <- x$get()
		inverse_matrix <- solve(original_matrix)
		message("Inverse Calculated")
		x$setInverse(inverse_matrix)
	}
}
