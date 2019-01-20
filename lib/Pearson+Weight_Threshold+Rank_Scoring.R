load("ms_test.RData")
load("ms_train.RData")

load("movie_test.RData")
load("movie_train.RData")


# Pearson Correlation

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

pearson_corr_MS <- pearson_corr(MS_train)
pearson_corr_movie <- pearson_corr(movie_train)
save(pearson_corr_MS, file = "pearson_corr_MS.RData")
save(pearson_corr_movie, file = "pearson_corr_movie.RData")


######################## Weight(Absolute correlation) Threshold

corr_thresh <- function(mat, threshold){
  diag(mat) <- 0
  select_neighbor <- list()
  for (i in 1:nrow(mat)){
    select_neighbor[[i]] <- which(abs(mat[i,]) > threshold)
  }
  return(select_neighbor)
}
# output a list of neighbor's index for each active user
selected_neighbor_MS <- corr_thresh(pearson_corr_MS, 0.5)
selected_neighbor_movie <- corr_thresh(pearson_corr_movie, 0.5)

######################## Prediction

avg_dev_pred <- function(train_data, test_data, pearson_correlation, selected_neighbors){
  
  pred.matrix <- matrix(NA, nrow = nrow(train_data), ncol = ncol(train_data))
  avg_rate_a <- apply(train_data, 1, mean)
  #train_data[is.na(train_data)] <- 0
  #test_data[is.na(test_data)] <- 0
  
  for (a in 1:nrow(train_data)){
    rate_u <- train_data[selected_neighbors[[a]],]
    #avg_rate_u <- apply(data[selected_neighbors[[a]],], 1, mean)
    weight_u <- pearson_correlation[a,selected_neighbors[[a]]]
    if (length(selected_neighbors[[a]]) == 0) {
      pred.matrix[a,] = avg_rate_a[a]
    } else if (length(selected_neighbors[[a]]) == 1) {
      pred.matrix[a,] = avg_rate_a[a] + (rate_u-avg_rate_a[selected_neighbors[[a]]])*weight_u / sum(weight_u)
    } else {
      pred.matrix[a, ] <- avg_rate_a[a]+apply((rate_u-avg_rate_a[selected_neighbors[[a]]])*weight_u,2,sum)/sum(weight_u)
    }
    
  }
  colnames(pred.matrix) = colnames(train_data)
  rownames(pred.matrix) = rownames(train_data)
  return(pred.matrix[rownames(train_data) %in% rownames(test_data), 
                     colnames(train_data) %in% colnames(test_data)])
}

pred_MS <- avg_dev_pred(train_data = MS_train, 
                        test_data = MS_test,
                        pearson_correlation = pearson_corr_ms_tr, 
                        selected_neighbors = selected_neighbor)
dim(pred_ms)

##################### rank score

Rank_Score <- function(pred, true, d = 0.02, alpha = 5){
  rank_mat_pred <- ncol(pred)+1-t(apply(pred,1,function(x){return(rank(x,ties.method = 'first'))}))
  rank_mat_true <- ncol(true)+1-t(apply(true,1,function(x){return(rank(x,ties.method = 'first'))}))
  
  v_true <- ifelse(true-d >0 & is.na(true-d) == F, true-d, 0)
  R_a <- apply(1/(2^((rank_mat_pred-1)/(alpha-1)))*v_true,1,sum)
  R_a_max <- apply(1/(2^((rank_mat_true-1)/(alpha-1)))*v_true,1,sum)
  R <- 100*sum(R_a)/sum(R_a_max)
  return(R)
}

rank_score_MS <- Rank_Score(pred_ms, MS_test, d = 0.02, alpha = 5)
rank_score_movie <- Rank_Score(pred_movie, movie_test, d = 0.02, alpha = 5)

