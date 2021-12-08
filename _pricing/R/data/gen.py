import pandas as pd
import numpy as np
import random as rd
import math

EPOCH = 100

def genMortality(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            if j == min_age:
                curr = 10000
            elif j == max_age:
                curr = 0
            elif curr != 0:
                curr -= int(10000/(max_age - min_age)) + rd.randint(-100,120)
            if curr < 0:
                curr = 0
            run.append(curr)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("mortality.csv", index=False)


def genHealthState(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            if j == min_age:
                curr = 0
            elif j == max_age or curr < 0:
                curr = -1
            else:
                rand = rd.randint(0,1000)
                if rand < 17:
                    curr = -1
                elif rand < 700 and (j + min_age) < max_age:
                    curr = 0
                elif rand < 400 and (j + min_age) > max_age:
                    curr = 0
                elif not curr or curr > 2:
                    curr = rd.randint(0,3)
                else:
                    curr += rd.randint(-1,1)
            run.append(curr)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("health.csv", index=False)


def genInflation(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            rand = round(rd.uniform(0, 0.07), 3)
            run.append(rand)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("inflation.csv", index=False)


def genInterestRate(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            rand = round(rd.uniform(0, 0.03), 3)
            run.append(rand)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("interest.csv", index=False)
    
    
def genStockPrice(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            rand = round(np.random.normal(0.07, 0.2), 3)
            run.append(rand)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("stock.csv", index=False)

def genHouseValue(min_age, max_age):
    data = []
    for i in range(EPOCH):
        run = []
        for j in range(min_age, max_age + 1):
            rand = round(rd.uniform(-0.1, 0.4), 3)
            run.append(rand)
        data.append(run)
    df = pd.DataFrame(data)
    df.to_csv("house.csv", index=False)


if __name__ == "__main__":
    genMortality(30,130)
    genHealthState(30,130)
    genInflation(30,130)
    genInterestRate(30,130)
    genStockPrice(30,130)
    genHouseValue(30,130)

