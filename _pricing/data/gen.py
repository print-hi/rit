#%%
import pandas as pd
import numpy as np

EPOCH = 100

#%%
def genMortality(min_age, max_age, decline):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age):
            run.append()
        
    df = pd.DataFrame(data)
    print(df)
    df.to_csv("mortality.csv", index=False)

def genHealthState():
    pass

def genInflation():
    pass

def genStockPrice():
    pass

def genHouseValue():
    pass

def genInterestRate():
    pass

if __name__ == "__main__":
    genMortality()