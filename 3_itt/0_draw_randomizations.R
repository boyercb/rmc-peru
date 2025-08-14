set.seed(13490875)

# draw primary randomizations ---------------------------------------------

rmc$block <- as.numeric(fct_cross(factor(rmc$batch), factor(rmc$strata)))

ra <- declare_ra(
  N = nrow(rmc),
  blocks = rmc$block,
  prob = 0.5
)

perms <- obtain_permutation_matrix(ra)


# draw two-stage randomizations  ------------------------------------------

rmc$clus <- rmc$treatment * rmc$group
rmc$clus[rmc$treatment==0] <- max(rmc$group) + 1:sum(rmc$treatment==0)

perms_2stage <- obtain_2stage_permutation_matrix(rmc, rmc$batch, rmc$strata, R = 1000)

A <- conduct_group_ra(rmc, rmc$batch, rmc$treatment, randomize_control = TRUE)
rmc$clus[rmc$treatment==0] <- A[rmc$treatment ==0]
rmc$group[rmc$treatment==0] <- A[rmc$treatment ==0]
  
perms_2stage_control <- obtain_2stage_permutation_matrix(rmc, rmc$batch, rmc$strata, R = 1000, randomize_control = TRUE)

perms_group <- obtain_group_permutation_matrix(rmc, rmc$batch, R = 10)
