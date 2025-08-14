# wrapper for the main least squares estimator
main_estimator <-
  function(outcome,
           treatment = "treatment",
           covariates = NULL,
           data,
           weights = NULL,
           spec = "lin",
           fe = c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           ),
           clusters = NULL,
           se_type = "HC2") {
    
    if (spec == "lin" & !(is.null(covariates) & is.null(fe))) {
      adj_set <- c(treatment, covariates, fe, paste0(treatment, ":", c(covariates, fe)))
    } else {
      adj_set <- c(treatment, covariates, fe)
    }

    lm_robust(
      formula = reformulate(
        termlabels = adj_set,
        response = outcome
      ),
      data = data,
      weights = weights,
      clusters = clusters,
      se_type = se_type
    )
  }

# perform randomization inference
ri <-
  function(fit,
           perms,
           stat = function(x) x$statistic[2],
           treatment = "treatment",
           hypothesis = "two") {
    
    if (fit$clustered) { 
      if (nrow(perms$Z) != fit$nobs) {
        stop("Error: number of rows in permutations matrix not equal to number of observations in original fit.")
      }
      R <- ncol(perms$Z)
    } else {
      if (nrow(perms) != fit$nobs) {
        stop("Error: number of rows in permutations matrix not equal to number of observations in original fit.")
      }
      R <- ncol(perms)
    }
    
    outcome <- fit$outcome
    frame <- model.frame(fit)
    
    X <- as.matrix(frame[, -c(1, 2)])
    y <- frame[[outcome]]
    
    rstats <- sapply(1:R, function(i) {
      if (fit$clustered) {
        Z <- perms$Z[, i]
        A <- perms$A[, i]
        clus <- Z * A 
        clus[Z==0] <- max(A) + 1:sum(Z==0)
        clus <- as.integer(clus)
      } else {
        Z <- perms[, i]
        clus <- NULL
      }
      
      dmat <- cbind(1, Z, X, Z * X)
      
      rfit <-
        lm_robust_fit(
          y,
          dmat,
          cluster = clus,
          weights = NULL,
          se_type = fit$se_type,
          has_int = TRUE
        )
      
      stat(rfit)
    })
    
    if (hypothesis == "two") {
      pvalue <- mean(abs(rstats) > abs(stat(fit)))
    } else if (hypothesis == "upper") {
      pvalue <- mean(rstats > stat(fit))
    } else if (hypothesis == "lower") {
      pvalue <- mean(rstats < stat(fit))
    }
    
    return(
      list(
        fit = fit,
        obs = stat(fit),
        null = rstats,
        p.value = pvalue
      )
    )
  } 

conduct_group_ra <- 
  function(data, batch, Z, randomize_control = FALSE) {
    
    A <- vector(length = nrow(data))
    
    A[batch != 6] <- block_ra(
      blocks = as.numeric(fct_cross(factor(batch), factor(Z)))[batch != 6],
      prob_each = rep(0.2, 5),
      conditions = as.character(1:5)
    )
    
    A[batch == 6] <- block_ra(
      blocks = Z[batch == 6],
      prob_each = rep(0.5, 2),
      conditions = as.character(1:2)
    )
    
    A <- A + (as.numeric(batch) - 1) * 5
    
    if (randomize_control) {
      A[Z==0] <- A[Z==0] + max(A)
    } else {
      A[Z==0] <- 0
    }
   
    
    return(A)
  }

conduct_2stage_ra <- 
  function(data,
           batch,
           strata,
           randomize_control = FALSE) {
    
      Z <- block_ra(
        blocks = as.numeric(fct_cross(factor(batch), factor(strata))),
        prob = 0.5
      )
      
      A <- conduct_group_ra(data, batch, Z, randomize_control)
    
    return(list(Z = Z, A = A))
  }

obtain_2stage_permutation_matrix <- 
  function(data,
           batch,
           strata, 
           R = 1,
           randomize_control = FALSE) {
    
    Amat <- matrix(nrow = nrow(data), ncol = R)
    Zmat <- matrix(nrow = nrow(data), ncol = R)
    
    for (r in 1:R) {
      ra <- conduct_2stage_ra(data, batch, strata, randomize_control)
      
      Amat[, r] <- ra$A
      Zmat[, r] <- ra$Z
    }
    
    return(list(A = Amat, Z = Zmat))
  }



obtain_group_permutation_matrix <- 
  function(data,
           batch,
           treatment = "treatment",
           R) {
    
    Amat <- matrix(nrow = nrow(data), ncol = R)

    for (r in 1:R) {
      ra <- conduct_group_ra(data, batch, data[[treatment]])
      
      Amat[, r] <- ra
    }
    
    return(Amat)
  }

peer_ri <- 
  function(outcome,
           covariates = NULL,
           treatment = "treatment",
           peer_exposure = "any_sexual_bl",
           group = "group",
           batch = "batch",
           strata = "strata",
           data,
           perms,
           stage = 2,
           randomize_control = FALSE,
           se_type = "CR2",
           subgroup = NULL,
           fe = c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           ),
           peer_func = function(data, group, peer_exposure) {
             data |>
               group_by(.data[[group]]) |>
               mutate(
                 ret = (
                   sum(.data[[peer_exposure]], na.rm = TRUE) - .data[[peer_exposure]]
                 ) / (n() - 1)
               ) |>
               ungroup() |>
               mutate(
                 ret = ifelse(
                   treatment==0, 
                   #0,
                   (ret - min(ret)) / (max(ret) - min(ret)),
                   (ret - min(ret)) / (max(ret) - min(ret))
                 )
               ) |>
               pull(ret)
           },
           stat = function(x) x$statistic[c(2,3)],
           hypothesis = "two") {
    
    # get design matrix and outcome
    if (is.null(subgroup)) {
      df <- data[, c(outcome, treatment, peer_exposure, covariates, group, batch, strata, fe, "id_status_w")]
      
    } else {
      df <-
        data[, unique(
          c(
            outcome,
            treatment,
            peer_exposure,
            covariates,
            group,
            batch,
            strata,
            fe,
            "id_status_w",
            all.vars(parse(text = subgroup))
            )
          )]
    }
    
    # get total number of permutations
    if (stage == 1) {
      R <- ncol(perms)
    } else {
      R <- ncol(perms$Z)
    }
    
    # loop over perms 
    for (r in 0:R) {
      
      if (r == 0) {
        # get current perm
        T1 <- df[[treatment]]
        A <- df[[group]]
        
      } else {
        # get current perm
        if (stage == 2) {
          T1 <- perms$Z[, r]
          A <- perms$A[, r]

        } else {
          T1 <- T1
          A <- perms[, r]
          
        }
        
      }
    
      # recalculate groups
      if (randomize_control) {
        df$group_id <- A 
        df$clus <- A 
      } else {
        df$group_id <- A 
        df$clus <- A * T1
        df$clus[T1==0] <- max(A) + 1:sum(T1==0)
      }
     
        
      # recalculate peer exposure under perm
      T2 <- peer_func(df, "group_id", peer_exposure)
      
      # form design matrix
      if (stage == 2) {
        if (randomize_control) {
          mat <- cbind(`(Intercept)` = 1, T1, T2, `T1T2` = T1 * T2, as.matrix(df[, c(covariates, fe)]))
          fn <- reformulate( 
            termlabels = c("T1", "T2", "T1T2", covariates, fe),
            response = outcome
          )
        } else {
          mat <- cbind(`(Intercept)` = 1, T1, `T1T2` = T1 * T2, as.matrix(df[, c(covariates, fe)]))
          fn <- reformulate(
            termlabels = c("T1", "T1T2", covariates, fe),
            response = outcome
          )
        }
        
      } else {
        mat <- cbind(`(Intercept)` = 1, `T1T2` = T1 * T2, as.matrix(df[, c(covariates, fe)]))
        fn <- reformulate(
          termlabels = c("T1T2", covariates, fe),
          response = outcome
        )
      }
      
      # limit to proper frame
      if (is.null(subgroup)) {
        ind <- df$id_status_w == 1 & !is.na(df[[outcome]])
      } else {
        ind <- df$id_status_w == 1 & !is.na(df[[outcome]]) & eval(parse(text=subgroup), envir = df)
      }
      if (stage == 1) {
        ind <- !is.na(df[[outcome]]) & df[[treatment]] == 1
        
        if (!is.null(subgroup)) {
          ind <- ind & eval(parse(text=subgroup), envir = df)
        }
      }
      
      
      mat <- mat[ind, ]
      y <- df[[outcome]][ind]
      clus <- as.integer(df$clus[ind])
      
      # re-estimate effects
      # fit <- 
      #   lm_robust_fit(
      #     y, 
      #     mat,
      #     cluster = clus,
      #     weights = NULL,
      #     se_type = se_type,
      #     has_int = TRUE
      #     # return_fit = FALSE
      #     # return_vcov = FALSE
      #   )
      odf <- as.data.frame(cbind(mat, y))
      names(odf)[ncol(odf)] <- outcome
      fit <- lm_robust(
        formula = fn,
        data = odf,
        cluster = clus,
        se_type = se_type,
        return_vcov = FALSE
      )

      # extract stats of interest
      if (r == 0) {
        
        ostats <- stat(fit)
        
        odf <- as.data.frame(cbind(mat, y))
        names(odf)[ncol(odf)] <- outcome
        ofit <- lm_robust(
          formula = fn,
          data = odf,
          cluster = clus,
          se_type = se_type,
          return_vcov = FALSE
        )
        #print(ofit)
        # initialize stats
        rstats <- matrix(nrow = R, ncol = length(ostats))
      } else {
        rstats[r, ] <- stat(fit)
      }
      
    }
    
    # calculate RI p-values
    if (hypothesis == "two") {
      pvalue <-
        sapply(1:ncol(rstats), function(c)
          mean(abs(rstats[, c]) > abs(ostats[c])))
    } else if (hypothesis == "upper") {
      pvalue <-
        sapply(1:ncol(rstats), function(c)
          mean(rstats[, c] > ostats[c]))
    } else if (hypothesis == "lower") {
      pvalue <-
        sapply(1:ncol(rstats), function(c)
          mean(rstats[, c] < ostats[c]))
    }
    
    return(
      list(
        fit = ofit,
        null = rstats,
        obs = ostats,
        p.value = pvalue
      )
    )
  } 
