# Computer lab 1

# Assignment 1

g <- function(x){
  y <- log(x+1)/(x^(3/2)+1)
  return(y)
}

## Task a: Plot the function

x <- seq(0,4,0.01)
plot(x, g(x), type="l", ylim=c(-0.2,1),col="blue")
grid()
# guess for the maximum point: little lower than 1
which.max(g(x)) * 0.01 - 0.01

## Task b: Compute derivative

# for latex:
# \frac{\frac{x^{\frac{3}{2}} + 1}{x + 1} - \frac{3 \sqrt{x} \ln\left(x + 1\right)}{2}}{\left(x^{\frac{3}{2}} + 1\right)^{2}}

dg <- function(x){
  nominator <- (x^(3/2)+1) / (x+1) - (log(x+1)*(3/2)*sqrt(x))
  denominator <- (x^(3/2)+1)^2
  y <- nominator/denominator
  return(y)
}



lines(x,dg(x), col="red")
lines(x,rep(0,length(x)))


## Task c: Implement bisection method
bisection <- function(fun, a, b, threshold){
  #' computes the maximum of a function using the bisection method
  #' 
  #' params:
  #' fun: derivative of the function
  #' a: starting point of initial interval
  #' b: last point of initial interval 
  #' threshold: convergence criterion
  
  # check if criterion for starting interval is met
  stopifnot(fun(a)*fun(b) < 0)
  
  xt <- (a + b)/2
  
  it <- 1 # start iterations counter with 1 as line above is first iteration
  
  # improve approximation until convergence criterion is met
  while(TRUE){
    it <- it + 1
    
    if(fun(a) * fun(xt) <= 0){
      b <- xt 
      #cat("New intervall:",a,b, "\n")
    }
    else{
      a <- xt
      #cat("New intervall:",a,b, "\n")
    }
    xt_next <- (a + b)/2
    
    if(abs((xt_next - xt)) < threshold){
      xt <- xt_next
      break
    }
    
    else{
      xt <- xt_next
      #cat("New xt:",xt,"\n")
    }
  }
  
  return(c(xt, it))
}

## Task d: Implement secant method

secant <- function(fun, x0, x1, threshold){
  #' computes the maximum of a function using the bisection method
  #' 
  #' params:
  #' fun: derivative of the function
  #' x0: initial x0 value for secant
  #' x1: initial x1 value for secant 
  #' threshold: convergence criterion
  
  it <- 0 # iterations counter
  
  while(TRUE){
    it <- it + 1
    x_next <- x1 - fun(x1) * (x1 - x0)/ (fun(x1) - fun(x0))
    
    if(abs((x1 - x0)) < threshold){
      break
    }
    else{
      x0 <- x1
      x1 <- x_next
      print(x1)
    }
  }
  
  return(c(x1, it))
}  
  
## Task e: Run the functions on different starting values
bisection(dg, 0.5,1.2,0.0001) # approximates good
bisection(dg, 0.5,0.8,0.0001) # doesn't work as both dg(x)are negative
bisection(dg, 0.1,3.5,0.0001) # works also for bigger starting interval

secant(dg, 0.5, 1.2, 0.0001) # finds a good approximation
secant(dg, 2.5, 3.2, 0.0001) # does not find the correct maximum as it approaches 0 for x -> inf
secant(dg, 1.1, 1.3, 0.0001) # approximates the value well even though both dg(x) are negative (difference to bisection method)
secant(dg, 0.1, 0.3, 0.0001) # finds the correct approximation

# comparison in terms of iterations and programming effort:
# bisection takes more (roughly twice as many) iterations compared to secant (13-16 compared to 7-8)
# the programming effort was similar for both methods

## Task f: Which method to prefer?
# As both methods have problems to find the maximum for some starting values but the secant method converges faster,
# we would choose secant. 
# In general we could start with the bisection method. If we start with a pair that meets the starting criterion it is 
# guaranteed to find a maximum. When the interval is narrowed down we can switch to the secant method for faster convergence. 
# If it fails to converge in this interval we could switch back to the bisection method. 




# Assignment 2

## Task a: write function for variance
myvar <- function(x){
  n <- length(x)
  v <- 1/(n-1) * (sum(x^2) - 1/n * (sum(x)^2))
  return (v)
}


## Task b: generate vector
set.seed(12345)
x <- rnorm(10000, mean=10^8, sd=sqrt(1))

## Task c: compute difference from custom to built-in variance for subsets of vector
diff <- numeric(10000)
for (i in 1:10000){
  x_sub <- x[1:i]
  diff[i] <- myvar(x_sub) - var(x_sub)
}
plot(diff)
abline(h = 0, col = "red", lty = 2)

## Task d: Improved variance function
two_pass <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  v <- sum((x - mean_x)^2) / (n - 1)
  return(v)
}

two_pass_diff <- numeric(10000)

for (i in 1:10000) {
  Xi <- x[1:i]
  two_pass_diff[i] <- two_pass(Xi) - var(Xi)
}

# Plot the comparison for two_pass
plot(1:10000, two_pass_diff, xlab = "i", ylab = "Yi", main = "Difference Yi = two_pass(Xi) - var(Xi)")
abline(h = 0, col = "red", lty = 2)

sum(x)^2
sum(x^2)
sum(x)^2 + sum(x^2)
x
2^1023
