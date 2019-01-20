##Transform Data MS
Transform_ms <- function(data){
  case_line <- which(data$V1 == 'C')
  user_id <- data$V3[case_line]
  vote_id <- sort(unique(data$V2[which(data$V1 == 'V')]))
  table <- matrix(0, nrow = length(user_id), ncol = length(vote_id))
  for(i in 1:length(case_line)){
    start_num <- case_line[i]
    end_num <- ifelse(i < length(case_line), case_line[i+1], nrow(data)+1)
    for(index in (start_num+1):(end_num-1)){
      j <- which(vote_id == data[index,3])
      table[i,j] <- 1
    }
  }
  rownames(table) <- as.character(user_id)
  colnames(table) <- as.character(vote_id)
  return(table)
}

##Transform movie data
Transform_m <- function(data){
  rows <- sort(unique(data$User))
  columns <- sort(unique(data$Movie))
  table <- matrix(NA, nrow = length(rows), ncol = length(columns))
  for(index in 1:length(rows)){
    row.name <- rows[index]
    i <- which(data$User == row.name)
    movies <- data[i,2]
    scores <- data[i,4]
    j <- which(columns %in% movies)
    table[index,j] <- (scores-1)
  }
  rownames(table) <- rows
  colnames(table) <- columns
  return(table)
}

## Similarity Weight: Pearson Correlation

pearson_corr <- function(data){
  corr_mat <- matrix(NA,nrow=nrow(data),ncol=nrow(data))
  colnames(corr_mat) <- rownames(data)
  rownames(corr_mat) <- rownames(data)
  for (i in 1:nrow(data)){
    for (j in 1:nrow(data)){
      index <- (!is.na(data[i,]))&(!is.na(data[j,]))
      if(sum(index)==0){
        corr_mat[i,j] <- 0
      }else{
        corr_mat[i,j] <- cor(data[i,index],data[j,index],method='pearson')
      }
    }
  }
  return(corr_mat)
}



## Similarity Weight: mean-squared-difference function

MSD_Weight <- function(data_mat){
  nuser <- nrow(data_mat)
  users <- rownames(data_mat)
  diff_mat <- matrix(NA,nrow=nuser,ncol=nuser)
  colnames(diff_mat) <- users
  rownames(diff_mat) <- users
  for (i in 1:nuser){
    for (j in 1:nuser){
      ri <- data_mat[i,]
      rj <- data_mat[j,]
      index <- (!is.na(ri))&(!is.na(rj))
      if(sum(index)==0){diff_mat[i,j] <- 0}
      else{
        ri_new <- data_mat[i,index]
        rj_new <- data_mat[j,index]
        diff_mat[i,j] <- mean((ri_new-rj_new)^2)
      }
    }
  }
  L <- max(diff_mat)
  weight_mat <- (L-diff_mat)/L
  return(weight_mat)
}


## Similarity Weight: Weight(Absolute correlation) Threshold

corr_thresh <- function(mat, threshold){
  diag(mat) <- 0
  select_neighbor <- list()
  for (i in 1:nrow(mat)){
    select_neighbor[[i]] <- which(abs(mat[i,]) > threshold)
  }
  return(select_neighbor)
}
# output a list of neighbor's index for each active user
# selected_neighbor_MS <- corr_thresh(pearson_corr_MS, 0.5)
# selected_neighbor_movie <- corr_thresh(pearson_corr_movie, 0.5)




## Similarity Weight: SimRank for data1

simrank <- function(data, c = 0.8, iter = 5){
  #First, we construct a matrix to save the the connections of each user.
  
  nr <- nrow(data)
  nc <- ncol(data)
  
  # Assign the weight to each user that connects to all items.
  user_list = list()
  for(i in 1:nr){
    vec <- rep(0, nc)
    vec[!is.na(data[i, ])] <- 1/sum(!is.na(data[i, ]))
    user_list[[i]] <- vec
  }
  
  user_conn <- matrix(0, nrow = nr, ncol = nc)
  
  for(i in 1:nr){
    user_conn[i, ] <- user_list[[i]]
  }
  
  #Second, we construct a matrix to save the the connections of each item.
  
  item_list = list()
  # Assign the weight to each user that connects to all items.
  for(i in 1:nc){
    vec <- rep(0, nr)
    vec[!is.na(data[, i])] <- 1/sum(!is.na(data[, i]))
    item_list[[i]] <- vec
  }
  
  item_conn <- matrix(0, nrow = nc, ncol = nr)
  
  for(i in 1:nc){
    item_conn[i, ] <- item_list[[i]]
  }
  #Third, we construct a big matrix to same all the connections between users and items as the weight, that is 1/I(a)I(b)
  # We make the upper part of the big matrix.
  matrix_conn_upp <- cbind(matrix(0, nrow = nr, ncol = nr), user_conn)
  # We make the lower part of the big matrix.
  matrix_conn_low <- cbind(item_conn, matrix(0, nrow = nc, ncol = nc))
  # Hence, we get the big matrix 
  matrix_conn <- rbind(matrix_conn_upp, matrix_conn_low)
  # Now, we build the sim rank function
  sim_matrix <- matrix(0, nrow = nr + nc , ncol = nc + nr)
  diag(sim_matrix) <- 1 # initial simrank matrix with diagnal = 1
  
  # Finally we run the iteration
  sim_list <- list()
  for(i in 1:iter){
    sim_matrix <- c * (matrix_conn %*% sim_matrix %*% t(matrix_conn))
    diag(sim_matrix) <- 1
    sim_list[[i]] <- sim_matrix
    
  }
  return(sim_matrix)
}


## Selecting Neighborhoods:Weight(Absolute correlation) Threshold

corr_thresh <- function(mat, threshold){
  diag(mat) <- 0
  select_neighbor <- list()
  for (i in 1:nrow(mat)){
    select_neighbor[[i]] <- which(abs(mat[i,]) > threshold)
  }
  return(select_neighbor)
}

# # output a list of neighbor's index for each active user



## Selecting Neighborhoods: best-n neighbors function

Select_BNN <- function(weight_mat, nnbors){
  nuser <- nrow(weight_mat)
  users <- rownames(weight_mat)
  nbor_mat <- matrix(NA, nrow = nuser, ncol = nnbors)
  rownames(nbor_mat) <- users
  
  for (i in 1:nuser){
    nbor_mat[i,] <- colnames(weight_mat)[order(weight_mat[i,],decreasing=T)][1:nnbors]
  }
  return(nbor_mat)
}



## Selecting Neighborhoods - Correlation Thresholding & Best-n-estimator
combine <- function(weight_mat, threshold, n){
  diag(weight_mat) <- 0
  thresh_neighbor <- list()
  select_neighbor <- list()
  nuser <- nrow(weight_mat)
  
  for (i in 1:nuser){
    thresh_neighbor[[i]] <- which(abs(weight_mat[i,]) > 0.5)
    if (length(thresh_neighbor[[i]]) < n) {
      select_neighbor[[i]] <- thresh_neighbor[[i]]
    } else {
      select_neighbor[[i]] <- 
        which(rownames(weight_mat) %in% 
                names((sort(weight_mat[i,thresh_neighbor[[i]]],decreasing=T))[1:n]))
    }
  }
  return(select_neighbor)
}


## Selecting Neighborhoods:Weight(Absolute correlation) Threshold

corr_thresh <- function(mat, threshold){
  diag(mat) <- 0
  select_neighbor <- list()
  for (i in 1:nrow(mat)){
    select_neighbor[[i]] <- which(abs(mat[i,]) > threshold)
  }
  return(select_neighbor)
}

# # output a list of neighbor's index for each active user



## Selecting Neighborhoods: best-n neighbors function

Select_BNN <- function(weight_mat, nnbors){
  nuser <- nrow(weight_mat)
  users <- rownames(weight_mat)
  nbor_mat <- matrix(NA, nrow = nuser, ncol = nnbors)
  rownames(nbor_mat) <- users
  
  for (i in 1:nuser){
    nbor_mat[i,] <- colnames(weight_mat)[order(weight_mat[i,],decreasing=T)][1:nnbors]
  }
  return(nbor_mat)
}



## Selecting Neighborhoods - Combine- Correlation Thresholding & Best-n-estimator
combine <- function(weight_mat, threshold, n){
  diag(weight_mat) <- 0
  thresh_neighbor <- list()
  select_neighbor <- list()
  nuser <- nrow(weight_mat)
  
  for (i in 1:nuser){
    thresh_neighbor[[i]] <- which(abs(weight_mat[i,]) > 0.5)
    if (length(thresh_neighbor[[i]]) < n) {
      select_neighbor[[i]] <- thresh_neighbor[[i]]
    } else {
      select_neighbor[[i]] <- 
        which(rownames(weight_mat) %in% 
                names((sort(weight_mat[i,thresh_neighbor[[i]]],decreasing=T))[1:n]))
    }
  }
  return(select_neighbor)
}



######################## W-T Prediction

avg_dev_pred <- function(train_data, test_data, pearson_correlation, selected_neighbors){
  
  pred.matrix <- matrix(NA, nrow = nrow(train_data), ncol = ncol(train_data))
  avg_rate_a <- apply(train_data, 1, mean)
  #train_data[is.na(train_data)] <- 0
  #test_data[is.na(test_data)] <- 0
  
  for (a in 1:nrow(train_data)){
    rate_u <- train_data[selected_neighbors[[a]],]
    #avg_rate_u <- apply(data[selected_neighbors[[a]],], 1, mean)
    weight_u <- pearson_correlation[a,selected_neighbors[[a]]]
    if (length(selected_neighbor[[a]]) == 0) {
      pred.matrix[a,] = avg_rate_a[a]
    } else if (length(selected_neighbor[[a]]) == 1) {
      pred.matrix[a,] = avg_rate_a[a] + 
        (rate_u-avg_rate_a[selected_neighbor[[a]]])*weight_u / sum(weight_u)
    } else {
      pred.matrix[a, ] <- avg_rate_a[a]+
        apply((rate_u-avg_rate_a[selected_neighbor[[a]]])*weight_u,2,sum)/sum(weight_u)
    }
    
  }
  colnames(pred.matrix) = colnames(train_data)
  rownames(pred.matrix) = rownames(train_data)
  return(pred.matrix[rownames(train_data) %in% rownames(test_data),
                     colnames(train_data) %in% colnames(test_data)])
}



######################## Prediction
# Prediction: compute weighted average of z-scores

ZScore <- function(weight_mat, nbor_mat, data_mat, user, content){
  neighbors <- nbor_mat[user,]
  numer <- rep(NA, length(neighbors))
  for (u in 1:length(neighbors)){
    nbor <- neighbors[u]
    r_ui <- data_mat[nbor, content]
    r_u <- mean(data_mat[nbor,], na.rm = T)
    sd_u <- sd(data_mat[nbor,], na.rm = T)
    w_au <- weight_mat[user, nbor]
    num <- (r_ui - r_u)/sd_u * w_au
    if (is.na(num) == FALSE){numer[u] <- (r_ui - r_u)/sd_u * w_au}
    else{numer[u] <- 0}
  }
  r_a <- mean(data_mat[user,], na.rm = T)
  sd_a <- sd(data_mat[user,], na.rm = T)
  w_a <- sum(weight_mat[user, neighbors])
  p_ai <- r_a + sd_a * sum(numer)/w_a
  
  return(p_ai)
}


ZScore_Mat <- function(weight_mat, nbor_mat, train_data, test_data){
  r <- nrow(test_data)
  c <- ncol(test_data)
  users <- rownames(test_data)
  contents <- colnames(test_data)
  ZScore_mat <- matrix(NA, r, c)
  for (i in 1:r){
    for (j in 1:c){
      if (is.na(test_data[i,j]) == F){
        ZScore_mat[i,j] <- 
          ZScore(weight_mat, nbor_mat, train_data, users[i], contents[j])
      }
    }
  }
  return (ZScore_mat)
}

##############################prediction simrank
ZScore_Mat_sr <- function(weight_mat, nbor_mat, train_data, test_data){
  
  #simrank_w_1 <- read.csv("simrank_MS_train.csv")
  weight_mat <- weight_mat[, -1]
  rownames(weight_mat) <- colnames(weight_mat)
  
  rownames(test_data) <- rownames(simrank_w_1)[1:nrow(test_data)]
  rownames(train_data) <- rownames(simrank_w_1)
  
  r <- nrow(test_data)
  c <- ncol(test_data)
  users <- rownames(test_data)
  contents <- colnames(test_data)
  ZScore_mat <- matrix(NA, r, c)
  for (i in 1:r){
    for (j in 1:c){
      if (is.na(test_data[i,j]) == F){
        ZScore_mat[i,j] <- ZScore(weight_mat, nbor_mat, train_data, users[i], contents[j])
      }
    }
  }
  return (ZScore_mat)
}

# Evaluation 1: ranked scoring function
##################### rank score

Rank_Score <- function(pred, true, d = 0.02, alpha = 5){
  rank_mat_pred <- ncol(pred)+1-t(apply(pred,1,function(x)
    {return(rank(x,ties.method = 'first'))}))
  rank_mat_true <- ncol(true)+1-t(apply(true,1,function(x)
    {return(rank(x,ties.method = 'first'))}))
  
  v_true <- ifelse(true-d >0 & is.na(true-d) == F, true-d, 0)
  R_a <- apply(1/(2^((rank_mat_pred-1)/(alpha-1)))*v_true,1,sum)
  R_a_max <- apply(1/(2^((rank_mat_true-1)/(alpha-1)))*v_true,1,sum)
  R <- 100*sum(R_a)/sum(R_a_max)
  return(R)
}



# Evaluation 2: mean absolute error (MAE) function

MAE <- function(pred, true){
  mae <- sum(abs(pred-test),na.rm = T)/sum(!is.na(abs(pred-test)))
  return(mae)
}

