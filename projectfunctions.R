pqnumber <- function(sign, p, q, nums) {
  sign <- as.integer(sign)
  p <- as.integer(p)
  q <- as.integer(q)
  nums <- as.integer(nums)
  if(abs(sign) != 1 || length(sign) != 1) {
    stop("argument 'sign' can only be 1 or -1")
  }
  if(length(p) != 1 || p < 0) {
    stop("argument `p` has to be a single integer greater than zero")
  }
  if(length(q) != 1 || q < 0) {
    stop("argument `q` has to be a single integer greater than zero")
}
  if(min(nums) < 0 || max(nums) > 9 || p+q+1 != length(nums)) {
    stop("argument `nums` needs to be a vector of p+q+1 integers between 0 to 9")
  }
  x <- structure(list(sign = sign, p = p, q = q, nums = nums), class = "pqnumber")
  return(x)
} 
is_pqnumber <- function(x) {
  class(x) == "pqnumber"
}
print.pqnumber <- function(x) {
  if (class(x) != "pqnumber") {
    stop("x must have class `pqnumber`")
  }
  rep <- -x[[2]]:x[[3]]
  total = 0
  for (i in seq_len(length(rep))) {
    total = total + x[[4]][i] * 10^rep[i]
  }
  toString(x[[1]]*total)
}
as_pqnumber <- function(x, p, q) {
  x <- unlist(strsplit(format(as.numeric(x), nsmall = p), ""))
  x <- x[x != "."]
  rep <- -p : q
  x <- rev(x)
  while (length(x) < length(rep)) {
    x <- c(x, "0")
  }
  x
}
as_numeric <- function(x) {
  UseMethod("as_numeric")
}
as_numeric.pqnumber <- function(x) {
  if (class(x) != "pqnumber") {
    stop("x must have class `pqnumber`")
  }
  rep <- -as.numeric(x[[2]]):as.numeric(x[[3]])
  total = 0
  for (i in seq_len(length(rep))) {
    total = total + as.numeric(x[[4]])[i] * 10^rep[i]
  }
  toString(as.numeric(x[[1]])*total)
}
`+.pqnumber` <- function(x, y) {
  if (class(x) != "pqnumber" || class(y) != "pqnumber") {
    stop("input x and y needs to be pq numbers")
  }
  # case where x and y are both positive or x and y are both negative
  if(as.numeric(as_numeric(x))*as.numeric(as_numeric(y)) > 0) {
    sign <- 1
    x_nums <- x$nums
    y_nums <- y$nums
    sum <- numeric(0)
    # carry over
    for (i in seq_len(length(x_nums))) {
      if ((x_nums[i] + y_nums[i]) <= 9) {
        sum <- c(sum, x_nums[i] + y_nums[i])
      }
      if ((x_nums[i] + y_nums[i]) > 9) {
        sum <- c(sum, (x_nums[i]+y_nums[i]) %% 10)
        x_nums[i+1] <- x_nums[i+1]+1
      }
    }
  }
  # case x is pos. and y is neg.
  if (x$sign == 1 && y$sign == -1) {
    # abs of x is larger than abs y
    if (abs(as.numeric(as_numeric(x))) > abs(as.numeric(as_numeric(y)))){
      sign <- 1
      x_nums <- x$nums
      y_nums <- y$nums
      sum <- numeric(0)
      #carry over
      for (i in seq_len(length(x_nums))) {
        if ((x_nums[i] - y_nums[i]) < 0) {
          x_nums[i+1] <- x_nums[i+1]-1
          sum <- c(sum, x_nums[i] + 10 - y_nums[i])
        }
        if ((x_nums[i] - y_nums[i]) >= 0) {
          sum <- c(sum, x_nums[i] - y_nums[i])
        }
      }
    } else {
      sign <- -1
      x_nums <- x$nums
      y_nums <- y$nums
      sum <- numeric(0)
      # carry over
      for (i in seq_len(length(x_nums))) {
        if ((y_nums[i] - x_nums[i]) < 0) {
          y_nums[i+1] <- y_nums[i+1]-1
          sum <- c(sum, y_nums[i] + 10 - x_nums[i])
        }
        if ((y_nums[i] - x_nums[i]) >= 0) {
          sum <- c(sum, y_nums[i] - x_nums[i])
        }
      }
    }
  }
  # test for overflow
  if (any(is.na(x_nums))) {
    warning("an overflow")
  }
  print.pqnumber(pqnumber(sign = sign, p = x$p, q = x$q, nums = sum))
}
`-.pqnumber` <- function(x, y) {
  if (class(x) != "pqnumber" || class(y) != "pqnumber") {
    stop("input x and y needs to be pq numbers")
  }
  # x and y positive numbers
  if(x$sign == 1 && y$sign == 1) {
    sign <- 1
    x_nums <- x$nums
    y_nums <- y$nums
    sub <- numeric(0)
    # carry over
    for (i in seq_len(length(x_nums))) {
      if ((x_nums[i] - y_nums[i]) < 0) {
        x_nums[i+1] <- x_nums[i+1]-1
        sub <- c(sub, x_nums[i] + 10 - y_nums[i])
      }
      if ((x_nums[i] - y_nums[i]) >= 0) {
        sub <- c(sub, x_nums[i] - y_nums[i])
      }
    }
  }
  # x and y are negative
  if (x$sign == -1 && y$sign == -1) {
    # y is more negative than x
    if (as.numeric(as_numeric(x)) > as.numeric(as_numeric(y))) {
      sign <- 1
      x_nums <- x$nums
      y_nums <- y$nums
      sub <- numeric(0)
      # carry over
      for (i in seq_len(length(x_nums))) {
        if ((y_nums[i] - x_nums[i]) < 0) {
          y_nums[i+1] <- y_nums[i+1]-1
          sub <- c(sub, y_nums[i] + 10 - x_nums[i])
        }
        if ((y_nums[i] - x_nums[i]) >= 0) {
          sub <- c(sub, y_nums[i] - x_nums[i])
        }
      }
    } else {
      # x is more negative than y
      sign <- -1
      x_nums <- x$nums
      y_nums <- y$nums
      sub <- numeric(0)
      for (i in seq_len(length(x_nums))) {
        if ((x_nums[i] - y_nums[i]) < 0) {
          x_nums[i+1] <- x_nums[i+1]-1
          sub <- c(sub, x_nums[i] + 10 - y_nums[i])
        }
        if ((x_nums[i] - y_nums[i]) >= 0) {
          sub <- c(sub, x_nums[i] - y_nums[i])
        }
      }
    }
  }
  # x is negative and y is positive
  if (x$sign == -1 && y$sign == 1) {
    sign <- -1
    x_nums <- x$nums
    y_nums <- y$nums
    sub <- numeric(0)
    for (i in seq_len(length(x_nums))) {
      if ((x_nums[i] + y_nums[i]) <= 9) {
        sub <- c(sub, x_nums[i] + y_nums[i])
      }
      if ((x_nums[i] + y_nums[i]) > 9) {
        sub <- c(sub, (x_nums[i]+y_nums[i]) %% 10)
        x_nums[i+1] <- x_nums[i+1]+1
      }
    }
  }
  # x is pos, y is negative
  if (x$sign == 1 && y$sign == -1) {
    sign <- 1
    x_nums <- x$nums
    y_nums <- y$nums
    sub <- numeric(0)
    for (i in seq_len(length(x_nums))) {
      if ((x_nums[i] + y_nums[i]) <= 9) {
        sub <- c(sub, x_nums[i] + y_nums[i])
      }
      if ((x_nums[i] + y_nums[i]) > 9) {
        sub <- c(sub, (x_nums[i]+y_nums[i]) %% 10)
        x_nums[i+1] <- x_nums[i+1]+1
      }
    }
  }
  # test for overflow
  if (any(is.na(x_nums))) {
    warning("an overflow")
  }
  print.pqnumber(pqnumber(sign = sign, p = x$p, q = x$q, nums = sub))
}
`*.pqnumber` <- function(x, y) {
  mat <- matrix(nrow = length(y$nums), ncol = length(x$nums))
  for (i in seq_len(length(y$nums))){
    for (j in seq_len(length(x$nums))){
      mat[i,j] <- y$nums[i]*x$nums[j]
    }
  }
  #adjust mat for each row down, add a zero
  adj_mat <- matrix(nrow = length(y$nums), ncol = 2*length(x$nums)-1)
  adj_mat[1, ] <- c(mat[1,], rep(0,length(x$nums)-1))
  for (i in seq_len(length(x$nums)-1)){
    adj_mat[1+i,] <- c(rep(0,i),mat[1+i,],rep(0,length(x$nums)-(1+i)))
  }
  nums <- unlist(strsplit(str_extract(paste(colSums(adj_mat), sep = "", collapse = ""),
                    "[^0][0-9]*"), ""))[1:(x$p+x$q+1)]
  if (x$sign == 1 && y$sign == 1) {
    sign <- 1
  }
  if (x$sign == -1 && y$sign == -1){
    sign <- 1
  }
  if (x$sign == -1 && y$sign == 1){
    sign <- -1
  }
  if (x$sign == 1 && y$sign == -1){
    sign <- -1
  }
  print.pqnumber(pqnumber(sign = sign, p = x$p, q = x$q, nums = nums))
}
square_root <- function(a, tol = 1e-8, iter_max = 5, verbose = FALSE) {
  x <- a
  x_2 <- a
  num <- numeric(0)
  num_2 <- numeric(0)
  f <- function(x) {
    x^2 - a
  }
  # without iter_max
  while(abs(f(x_2)) > tol) {
    x_2 <- 0.5*(x_2 + a/x_2)
    num_2 <- c(num_2, x_2)
  }
  #with iter_max
  for (i in seq_len(iter_max)) {
    x <- 0.5*(x + a/x)
    num <- c(num, x)
  }
  # Compare with iter_max and without iter_max and return whichever reach the tol first
  if (length(num) < length(num_2)) {
    if (verbose) {
      (num)
    } else {
      (min(num))
    }
  } else {
    if (verbose) {
      (num_2)
    } else {
      (min(num_2))
    }
  }
}
nth_root <- function(a, n = 2, tol = 1e-8, iter_max = 20, verbose = FALSE) {
  # n = 0 returns undefined
  if (n == 0) {
    stop("0th root of any an arbitrary is undefined")
  }
  # n = 1 returns a
  if (n == 1) {
    return(a)
  }
  x <- a
  x_2 <- a
  num <- numeric(0)
  num_2 <- numeric(0)
  f <- function(x) {
    x^n - a
  }
  # without iter_max
  while(abs(f(x_2)) > tol) {
    x_2 <- (n-1)/n*x_2 + a/(n*x_2^(n-1))
    num_2 <- c(num_2, x_2)
  }
  # with iter_max
  for (i in seq_len(iter_max)) {
    x <- (n-1)/n*x + a/(n*x^(n-1))
    num <- c(num, x)
  }
  # Compare with iter_max and without iter_max and return whichever reach the tol first
  if (length(num) < length(num_2)) {
    if (verbose) {
      num
    } else {
      min(num)
    }
  } else {
    if (verbose) {
      num_2
    } else {
      min(num_2)
    }
  }
}
