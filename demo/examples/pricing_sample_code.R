# Account-Based Pension
ap <- create_policy_AP(1000000, 50000)
cf_ap <- simulate_cf(ap)
val_ap <- value_policy(ap, cf_ap)

# Life Annuity
la <- create_policy_LA(60000, 5, 0, 0.01)
cf_la <- simulate_cf(la)
val_la <- value_policy(la, cf_la)

# Pooled Annuity
pa <- create_policy_PA(60000, 10000, 0.05, 0.01)
cf_pa <- simulate_cf(pa)
val_pa <- value_policy(pa, cf_pa)

# Care Annuity
ca <- create_policy_CA(c(60000, 1200), c(0, 0.04), c(5, 0), c(0.04, 0.01))
cf_ca <- simulate_cf(ca)
val_ca <- value_policy(ca, cf_ca)

# Reverse Mortgage
rm <- create_policy_RM(600000, 0.64, 0.01, 0.04)
cf_rm <- simulate_cf(rm)
val_rm <- value_policy(rm, cf_rm)

# Variable Annuity (GMWB)
va <- create_policy_VA(1000000, 30, 0.1, 0.01)
cf_va <- simulate_cf(va)
val_va <- value_policy(va, cf_va)


# --------------------------------------------------------------------------------------------------
# NEW FUNCTIONALITY - STILL IN BETA
N_PATHS <- 100; MAX_YEARS <- 100
construct_state_matrix <- function(ages_at_death, max_years) {
    n_paths <- length(ages_at_death)
    state <- matrix(rep(-1, n_paths*max_years),nrow=n_paths,ncol=max_years,byrow=TRUE)
    for (i in seq(1, n_paths)) {
        death_yr <- ages_at_death[i]
        state[i,1:death_yr] <- rep(0, death_yr)
    }
    return(state)
}
construct_econ_var <- function(sdf, max_years) {
    n_paths <- length(sdf)/max_years
    sdf <- matrix(sdf,nrow=n_paths,ncol=max_years,byrow=TRUE)
    econ_var <- list(discount_factors=sdf)
    return(econ_var)
}

# ---- TEST 1 - ECON_VAR ---------------------------------------------------------------------------
sdf <- rep(1.06, N_PATHS*MAX_YEARS)
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)

la <- create_policy_LA(60000, defer = 5, increase = 0.06, loading = 0.06)
cf_la <- simulate_cf(la, n = 100, econ_var = econ_var)
val_la <- value_policy(la, cf_la)

# ---- TEST 2 - ECON_VAR & STATE  ------------------------------------------------------------------
sdf <- rep(1.06, N_PATHS*MAX_YEARS)
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)
la <- create_policy_LA(60000, defer = 5, increase = 0.06, loading = 0.06)

# VERSION 1 - REP 6
state <- construct_state_matrix(rep(6, N_PATHS), max_years = MAX_YEARS)

cf_la <- simulate_cf(la, n = 100, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)


# VERSION 2 - SEQ 1 to N
state <- construct_state_matrix(seq(1, N_PATHS), max_years = MAX_YEARS)

cf_la <- simulate_cf(la, n = 100, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)
