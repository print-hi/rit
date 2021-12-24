#%%
import pandas as pd
import numpy as np
import random as rd
import math

# %%
health = pd.read_csv("health3.csv")
for i in health:
    for index, j in enumerate(health[i]):
        if (j == 1):
            health[i][index] = rd.randint(1,3)

#%%
health.to_csv("health5.csv", index=False)

# %%
mort = pd.read_csv("mortality.csv")
agg = []
for i in mort:
    agg.append(1 + (sum(mort[i])/len(mort[i])))
agg = agg[1:]

pool_exp = [int(i * 10000) for i in agg]

#%%
df = pd.DataFrame(pool_exp)
df.to_csv("pool-exp.csv", index=False)

# %%
pool = []
for i in range(len(mort)):
    run = []
    alive = 1000
    for px in agg:
        if px == 0:
            alive = 0
        elif px == 1:
            alive = alive
        else: 
            alive = np.random.binomial(alive, px)
        run.append(alive)
    pool.append(run)
df = pd.DataFrame(pool)
df.to_csv("pool.csv", index=False)


# %%
