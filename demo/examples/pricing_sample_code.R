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
