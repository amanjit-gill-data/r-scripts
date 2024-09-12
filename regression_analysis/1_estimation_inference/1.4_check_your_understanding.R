# 1.4 Check Your Understanding

# turn off scientific notation
options(scipen = 999)

### DATA ###

Y_vect <- c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 
                        4063, 4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 
                        6473, 7501, 7886, 8108, 8546, 8666, 8831, 9106, 9711, 
                        9806, 10205, 10396, 10861, 11026, 11214, 11362, 11604,
                        11608, 11745, 11762, 11895, 12044, 13520, 13670, 14110, 
                        14496, 15395, 16179, 17092, 17568, 17568)

### FUNCTION DEF ###

np_table <- function(Y_vect, theta0, lambda, iters) {
  
  est_table <- data.frame(
    row.names = c('theta', 'U', 'Uprime', 'EUprime', 'UUprime', 'UEUprime')
  )
  
  N <- length(Y_vect)
  
  for (i in 1:(iters+1)) {
    
    col_name <- as.character(i)
    
    est_table['theta', col_name] <- theta0 
    est_table['U', col_name] <- -lambda*N/theta0 + lambda*sum(Y_vect**lambda)/(theta0**(lambda+1))
    est_table['Uprime', col_name] <- lambda*N/(theta0**2) - lambda*(lambda+1)*sum(Y_vect**lambda)/(theta0**(lambda+2))
    est_table['EUprime', col_name] <- -lambda**2 * N / theta0**2
    est_table['UUprime', col_name] <- est_table['U', col_name] / est_table['Uprime', col_name]
    est_table['UEUprime', col_name] <- est_table['U', col_name] / est_table['EUprime', col_name]
    
    theta0 <- theta0 - est_table['U', col_name]/est_table['Uprime', col_name]
  }
  
  est_table[2:4,] <- est_table[2:4,] * 10^6
  
  return(round(est_table, 2))
}

### EXERCISE ###

# estimate shape parameter theta when lambda = 2 and theta0 = mean
np_table(Y_vect, mean(Y_vect), 2, 3)

