# ---- HELPER FUNCTIONS  ---------------------------------------------------------------------------
construct_state_matrix <- function(ages_at_death, max_years) {
    n_paths <- length(ages_at_death)
    state <- matrix(rep(-1, n_paths*max_years),nrow=n_paths,ncol=max_years,byrow=TRUE)
    for (i in seq(1, n_paths)) {
        death_yr <- ages_at_death[i]
        state[i,1:death_yr] <- rep(0, death_yr)
    }
    return(state)
}

construct_state_matrix_from_probs <- function(surv_probs, max_years, n_paths) {
    ages_at_death <- c()
    for (i in seq(1, n_paths)) {
        p <- stats::runif(length(surv_probs))
        lv <- min(which((p > surv_probs) == TRUE))
        ages_at_death <- c(ages_at_death, lv)
    }

    print(ages_at_death)

    return(construct_state_matrix(ages_at_death, max_years))
}

construct_econ_var <- function(sdf, max_years, stock=NULL, infla=NULL) {
    n_paths <- length(sdf)/max_years
    sdf <- matrix(sdf,nrow=n_paths,ncol=max_years,byrow=TRUE)

    econ_var <- list(sdf=sdf)

    if(!is.null(stock)) {
        stock <- matrix(stock,nrow=n_paths,ncol=max_years,byrow=TRUE)
        li <- list(stock=stock)
        econ_var <- c(econ_var, li)
    }

    if(!is.null(infla)) {
        infla <- matrix(stock,nrow=n_paths,ncol=max_years,byrow=TRUE)
        li <- list(infla=infla)
        econ_var <- c(econ_var, li)
    }

    return(econ_var)
}

get_px_from_lx <- function(lx) {
    px <- lx
    px[1] <- 1
    for (i in seq(2, length(lx) - 1)) {
        px[i] <- 1 - (lx[i] - lx[i+1])/lx[i]
    }
    px[length(lx)] <- 0
    return(px)
}

# code from https://robotwealth.com/efficiently-simulating-geometric-brownian-motion-in-r/
simulate_gbm <- function(nsim = 100, t = 25, mu = 0, sigma = 0.1, S0 = 100, dt = 1./365) {
    epsilon <- matrix(rnorm(t*nsim), ncol = nsim, nrow = t)
    gbm <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    gbm <- apply(rbind(rep(S0, nsim), gbm), 2, cumprod)
    return(t(gbm))
}

# ---- TEST: BASE FUNCTIONALITY FOR ECON_VAR & STATE  ----------------------------------------------
N_PATHS <- 100; MAX_YEARS <- 100

# Test error case for input parameters
# - econ_var matrices have inconsistent dimensions (insufficient data) with provided state matrix
sdf <- rep(1.06, N_PATHS*(MAX_YEARS-1))
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS-1)
state <- construct_state_matrix(rep(6, N_PATHS), max_years = MAX_YEARS)

la <- create_policy_LA(60000, defer = 5, increase = 0.06)
cf_la <- simulate_cf(la, n = N_PATHS, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)

# Test for consistent results
# - all policyholders die after 6 years & discount rate = interest rate
sdf <- rep(1.06, N_PATHS*MAX_YEARS)
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)
state <- construct_state_matrix(rep(6, N_PATHS), max_years = MAX_YEARS)

la <- create_policy_LA(60000, defer = 5, increase = 0.06)
cf_la <- simulate_cf(la, n = N_PATHS, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)

# Test for consistent results
# - each subsequent policyholder die a year later than predecessor & discount rate = interest rate
sdf <- rep(1.06, N_PATHS*MAX_YEARS)
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)
state <- construct_state_matrix(seq(1, N_PATHS), max_years = MAX_YEARS)

la <- create_policy_LA(60000, defer = 5, increase = 0.06)
cf_la <- simulate_cf(la, n = N_PATHS, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)


# ---- TEST: ANNUITY VALUATION ---------------------------------------------------------------------
N_PATHS <- 100; MAX_YEARS <- 100

library(lifecontingencies); data(soaLt);

# Test for output against lifecontingencies (axn) at 6% vs manual calculation
soa08Act <- with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
surv <- extract_px_from_lx(lx08)

sdf <- rep(1.06, N_PATHS*MAX_YEARS)
econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)
state <- construct_state_matrix_from_probs(surv, MAX_YEARS, N_PATHS)

la <- create_policy_LA(1, defer = 0, increase = 0)
cf_la <- simulate_cf(la, n = N_PATHS, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)

# Comparison 1: manual calculation
benefits_paid <- state + 1
discount_fctr <- cumprod(rep(1.06, MAX_YEARS))
benefits_disc <- t(t(benefits_paid) / discount_fctr)
apv <- rowSums(benefits_disc)
mean(apv)

# Comparison 2: lifecontingencies
axn(soa08Act, x=65, payment="immediate")

# Test for output against axn in life contingencies
N_PATHS <- 100; MAX_YEARS <- 100

library(lifecontingencies); data(soaLt);

test_against_axn <- function() {
    obs <- c()
    exp <- c()
    for (i in seq(0.01, 0.20, 0.01)) {
        soa08Act <- with(soaLt, new("actuarialtable",interest=i,x=x,lx=Ix,name="SOA2008"))
        lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
        surv <- extract_px_from_lx(lx08)
        sdf <- rep(1 + i, N_PATHS*MAX_YEARS)
        econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS)
        state <- construct_state_matrix_from_probs(surv, MAX_YEARS, N_PATHS)
        la <- create_policy_LA(1, defer = 0, increase = 0)
        cf_la <- simulate_cf(la, n = N_PATHS, state = state, econ_var = econ_var)
        val_la <- value_policy(la, cf_la)

        obs <- c(obs, mean(val_la$values))
        exp <- c(exp, axn(soa08Act, x=65, payment="immediate"))
    }
    x <- seq(0.01, 0.20, 0.01)
    plot(x,obs,type="l",col="red", ylim = c(0, 30))
    lines(x,exp,col="green")
}
test_against_axn()

# ---- TEST: ACCOUNT BASED PENSION -----------------------------------------------------------------
N_PATHS <- 100; MAX_YEARS <- 100

library(lifecontingencies); data(soaLt);

# Test for against life annuity at 6% interest & 6% return
soa08Act <- with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
surv <- extract_px_from_lx(lx08)

state <- construct_state_matrix_from_probs(surv, MAX_YEARS, N_PATHS)

sdf <- rep(1.06, N_PATHS*MAX_YEARS)
infla <- rep(0, N_PATHS*MAX_YEARS)

stock_price <- cumprod(rep(1.06, MAX_YEARS))
stock <- rep(stock_price, MAX_YEARS)

econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS, stock = stock, infla = infla)

ap <- create_policy_AP(balance = 8.98, expense = 1)
cf_ap <- simulate_cf(ap, n = N_PATHS, state = state, econ_var = econ_var)
val_ap <- value_policy(ap, cf_ap)

# Test for against life annuity at 6% interest & 3% return
soa08Act <- with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
surv <- extract_px_from_lx(lx08)

state <- construct_state_matrix_from_probs(surv, MAX_YEARS, N_PATHS)

sdf <- rep(1.06, N_PATHS*MAX_YEARS)
infla <- rep(0, N_PATHS*MAX_YEARS)

stock_price <- cumprod(rep(1.03, MAX_YEARS))
stock <- rep(prices_over_time, MAX_YEARS)

econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS, stock = stock, infla = infla)

ap <- create_policy_AP(balance = 8.98, expense = 1)
cf_ap <- simulate_cf(ap, n = N_PATHS, state = state, econ_var = econ_var)
val_ap <- value_policy(ap, cf_ap)

# Test for against life annuity at 6% interest & GBM return (mu = 3%, sigma = 6%)
sizes <- c(10, 100, 1000, 5000)

val <- c()
for (n in sizes) {
    soa08Act <- with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
    lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
    surv <- extract_px_from_lx(lx08)

    state <- construct_state_matrix_from_probs(surv, MAX_YEARS, n)

    sdf <- rep(1.06, n*MAX_YEARS)
    infla <- rep(0, n*MAX_YEARS)

    stock <- simulate_gbm(nsim = n, t = MAX_YEARS - 1, mu = 0.06, sigma = 0.06, S0 = 1, dt = 1)

    econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS, stock = as.vector(stock), infla = infla)

    ap <- create_policy_AP(balance = 8.98, expense = 1)
    cf_ap <- simulate_cf(ap, n = n, state = state, econ_var = econ_var)
    val_ap <- value_policy(ap, cf_ap)

    val <- c(val, val_ap$stats$mean)
}

plot(sizes, val)

# ---- TEST: POOLED ANNUITY ------------------------------------------------------------------------
N_PATHS <- 1000; MAX_YEARS <- 100

library(lifecontingencies); data(soaLt);

# Test for output against provided spec for pool size of 100
soa08Act <- with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
lx08 <- soa08Act@lx[66:length(soa08Act@lx)]
surv <- extract_px_from_lx(lx08)
surv[length(surv)] <- 0

sdf <- rep(1.06, N_PATHS*MAX_YEARS)

stock_price <- cumprod(rep(1.06, MAX_YEARS))
stock <- rep(stock_price, MAX_YEARS)

econ_var <- construct_econ_var(sdf, max_years = MAX_YEARS, stock = stock)

state <- construct_state_matrix_from_probs(surv, MAX_YEARS, N_PATHS)

pa <- create_policy_PA(1, 100, 0.06)
cf_pa <- simulate_cf(pa, n = N_PATHS, state = state, econ_var = econ_var, cohort_death_probs = 1 - surv)
val_pa <- value_policy(pa, cf_pa)

# Test for output against provided spec for pool size of 1000
pa <- create_policy_PA(1, 1000, 0.06)
cf_pa <- simulate_cf(pa, n = N_PATHS, state = state, econ_var = econ_var, cohort_death_probs = 1 - surv)
val_pa <- value_policy(pa, cf_pa)

# Test for output against provided spec for pool size of 10000
pa <- create_policy_PA(1, 10000, 0.06)
cf_pa <- simulate_cf(pa, n = N_PATHS, state = state, econ_var = econ_var, cohort_death_probs = 1 - surv)
val_pa <- value_policy(pa, cf_pa)

# Test for output against provided spec for pool size of 10000 & 7% interest
stock_price <- cumprod(rep(1.07, MAX_YEARS))
stock <- rep(stock_price, MAX_YEARS)

pa <- create_policy_PA(1, 10000, 0.06)
cf_pa <- simulate_cf(pa, n = N_PATHS, state = state, econ_var = econ_var, cohort_death_probs = 1 - surv)
val_pa <- value_policy(pa, cf_pa)

# Test for output against provided spec for pool size of 10000 & 90& qx
stock_price <- cumprod(rep(1.06, MAX_YEARS))
stock <- rep(stock_price, MAX_YEARS)

state <- construct_state_matrix_from_probs(0.1 + 0.9 * surv, MAX_YEARS, N_PATHS)

pa <- create_policy_PA(1, 100, 0.06)
cf_pa <- simulate_cf(pa, n = N_PATHS, state = state, econ_var = econ_var, cohort_death_probs = 1 - surv)
val_pa <- value_policy(pa, cf_pa)



construct_state_matrix <- function(ages_at_death, max_years) {
    n_paths <- length(ages_at_death)
    state <- matrix(rep(-1, n_paths*max_years),
                    nrow=n_paths,
                    ncol=max_years,
                    byrow=TRUE)
    for (i in seq(1, n_paths)) {
        death_yr <- ages_at_death[i]
        state[i, 1:death_yr] <- rep(0, death_yr)
    }
    return(state)
}

# Construct state matrix
state <- construct_state_matrix(seq(1, 100), max_years = 100)

sdf <- matrix(rep(1.06, N_PATHS*MAX_YEARS),
              nrow=100,
              ncol=100,
              byrow=TRUE)

# Create list of relevant economic variables
econ_var <- list(sdf=sdf)

la <- create_policy_LA(1, defer = 0, increase = 0)
cf_la <- simulate_cf(la, n = 100, state = state, econ_var = econ_var)
val_la <- value_policy(la, cf_la)
