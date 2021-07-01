
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ULAM SPIRALS
# short script for generating Ulam spirals
# background: https://en.wikipedia.org/wiki/Ulam_spiral
# Ed Bannister github.com/foresteddy
#+++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. LOADING PACKAGES AND DEFINING TERMS
#+++++++++++++++++

rm(list = ls(all=TRUE)) 
library(plot.matrix)
library(fields)
library(RColorBrewer)

# FUNCTIONS BELOW ARE NEATEST IF A SQUARE NUMBER IS SPECIFIED
target_n <- 400
N <- target_n^2 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. FUNCTION FOR FINDING PRIME NUMBERS
# brute force - large N will need a more efficient algorithm
#+++++++++++++++++

prime_numbers <- function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  }
  else 
  {
    stop("Input number should be at least 2.")
  }
} 

#creates vector of the prime

pn <- prime_numbers(N)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. CREATING SPIRAL MATRIX OF THE NUMBERS
#+++++++++++++++++

#spiral matrix of the numbers
#this writes the numbers outside in

spiral_matrix <- function(n) {
  stopifnot(is.numeric(n))
  stopifnot(n > 0)
  steps <- c(1, n, -1, -n)
  reps <- n - seq_len(n * 2 - 1L) %/% 2
  indicies <- rep(rep_len(steps, length(reps)), reps)
  indicies <- cumsum(indicies)
  values <- integer(length(indicies))
  values[indicies] <- seq_along(indicies)
  matrix(values, n, n, byrow = TRUE)
}

#invert the matrix so it's written from inside out, as required by Ulam's method
vec_N <-rep(N,N)
vec_mat <- matrix(vec_N,nrow = sqrt(N),ncol = sqrt(N))
mat <- spiral_matrix(sqrt(N))
inv_mat <- vec_mat-mat

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PLOTTING ULAM SPIRAL OF PRIME NUMBERS UP TO N
#+++++++++++++++++

inv_mat<-as.integer(inv_mat %in% pn)
Num_mat <- matrix(inv_mat,nrow = sqrt(N),ncol = sqrt(N))

# the prime numbers are in colour
image(1:sqrt(N),1:sqrt(N),Num_mat, zlim=c(0,1), 
      col = c("white","steelblue4"), 
      xlab="", 
      ylab="", 
      main="Ulam spirals")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. CREATING SPIRAL MATRIX OF RANDOM ASSORTMENT OF 0 AND 1
#+++++++++++++++++

# 1s appear with same mean frequency as the the primes < N
# plot highlights lack of structure in random assortment vs. Ulam spirals

#calculate approximate probability of primes up to N
probN <- length(pn)/N

rand_vec <- sample(0:1,N,TRUE,prob = c((1-probN), probN))
rand_mat <- matrix(rand_vec,nrow = sqrt(N),ncol = sqrt(N))

image(1:sqrt(N),1:sqrt(N),rand_mat, zlim=c(0,1), 
      col = c("white","steelblue4"), 
      xlab="",
      ylab="", 
      main="Random on/off")
