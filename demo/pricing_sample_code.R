# ------------------------------------------------------------------------
# ---- Policy

ap <- create_policy_AP(2000000, 50000)
ca <- create_policy_CA(c(60000, 1200), c(0, 0.04), c(8, 0), c(0.04, 0.05))
la <- create_policy_LA(60000, 5, 0.02, 0.05)
pa <- create_policy_PA(60000, 10000, 0.05, 0.05)
rm <- create_policy_RM(100000, 0.4, 0.01, 0.05)
va <- create_policy_VA(100000, 30, 0.1, 0.005)

# ------------------------------------------------------------------------
# ---- Cashflow + Simulate

cf_ap <- simulate_cf(ap)
cf_ca <- simulate_cf(ca)
cf_la <- simulate_cf(la)
cf_pa <- simulate_cf(pa)
cf_rm <- simulate_cf(rm)
cf_va <- simulate_cf(va)

# ------------------------------------------------------------------------
# ---- Valuation

val_ap <- value_policy(ap, cf_ap)
val_ca <- value_policy(ca, cf_ca)
val_la <- value_policy(la, cf_la)
val_rm <- value_policy(rm, cf_rm)
val_va <- value_policy(va, cf_va)
val_pa <- value_policy(pa, cf_pa)
