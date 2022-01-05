#%%
import numpy as np
import pandas as pd

files = {
    "asx200.csv",
    "aud_index.csv",
    "cpi.csv",
    "gdp.csv",
    "home_index.csv",
    "mortgage_rate.csv",
    "rental_yield.csv",
    "zcp10y_spread.csv",
    "zcp3m_yield.csv"    
}

#%%
for name in files:
    data = pd.read_csv("var_simulated_" + name)
    trim = data[data.columns[2::4]]
    if data["2021 Q2"][1] > 10:
        trim = trim.round(2)
    else:
        trim = trim.round(4)
    trim.columns = [None] * len(trim.columns)
    trim.to_csv("extracted/" + name, index = False, header = False)
    
# %%
data = pd.read_csv("agg_mortality_simulated.csv")
data = data.drop(columns = ['Unnamed: 0'], axis = 1)

data.to_csv("extracted/mortality.csv", index = False, header = False)

# %%
data = pd.read_csv("health_state_simulated.csv")
data = data.drop(columns = ['Unnamed: 0', 'V100'], axis = 1)

data.to_csv("extracted/health3.csv", index = False, header = False)

# %%
for i in data:
    for index, j in enumerate(data[i]):
        if (j == 1):
            data[i][index] = np.random.randint(1,4)
            
data.to_csv("extracted/health5.csv", index = False, header = False)

# %%
mort = pd.read_csv("extracted/mortality.csv", header=None)
agg = []
for i in mort:
    agg.append(1 + (sum(mort[i])/len(mort[i])))
agg = agg[:]

pool_exp = [int(i * 10000) for i in agg]

pool_E = []

for i in mort:
    pool_E.append(pool_exp)
    
df = pd.DataFrame(pool_E)
df.to_csv("extracted/pool-exp.csv", index=False, header = False)

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
df.to_csv("extracted/pool.csv", index=False, header = False)

pool_e = pd.read_csv("extracted/pool-exp.csv", header=None)
pool = pd.read_csv("extracted/pool.csv", header=None)

#%%
files = {
    "extracted/asx200.csv",
    "extracted/aud_index.csv",
    "extracted/cpi.csv",
    "extracted/gdp.csv",
    "extracted/health3.csv",
    "extracted/health5.csv",
    "extracted/home_index.csv",
    "extracted/mortality.csv",
    "extracted/mortgage_rate.csv",
    "extracted/pool-exp.csv",
    "extracted/pool.csv",
    "extracted/rental_yield.csv",
    "extracted/zcp3m_yield.csv",
    "extracted/zcp10y_spread.csv"    
}

for f in files:
    check = pd.read_csv(f, header=None)
    try:
        assert(check.shape == (10000, 100))
    except Exception as e:
        print(f)
    
# %%
