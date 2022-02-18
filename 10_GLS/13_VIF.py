# Reference: https://github.com/RanjithRShetty/Variance-inflation-factor-VIF-/blob/master/VIF.ipynb

# VIF-Variance inflation factor

# %matplotlib inline
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tools.tools import add_constant

data=pd.read_csv("your_file.csv")
df=pd.DataFrame(data)
X=add_constant(df)
X.head()


pd.Series([variance_inflation_factor(X.values, i)
         for i in range(X.shape[1])],
          index=X.columns)

          
          
          
          