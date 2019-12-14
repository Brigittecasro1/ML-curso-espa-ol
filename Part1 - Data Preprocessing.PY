# -*- coding: utf-8 -*-
"""
Created on Thu Dec 12 22:32:43 2019

@author: karen
"""
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

#importar el dataset
dataset = pd.read_csv("Data.csv")
X = dataset.iloc[:,:-1].values
y = dataset.iloc[:,3].values

#tratamiento de los NA'S
from sklearn.impute import SimpleImputer as Simp
Imputer = Simp(missing_values= np.nan , strategy = "mean", verbose =0)
Imputer.fit(X [:, 1:3])
X [:, 1:3] = Imputer.transform(X [:, 1:3])

# Codificar datos categóricos
from sklearn import preprocessing
le_X = preprocessing.LabelEncoder()
X[:,0] = le_X.fit_transform(X[:,0])

#one hot encoder y crear variables dummy en la primera columna:
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import make_column_transformer
onehotencoder = make_column_transformer((OneHotEncoder(), [0]), remainder = "passthrough")
X = onehotencoder.fit_transform(X)

le_y = preprocessing.LabelEncoder()
y = le_y.fit_transform(y)

#Dividir el Data set en conjuntos de training y test
from sklearn.model_selection import train_test_split as tts
X_train, X_test, y_train, y_test = tts(X, y, test_size = 0.2, random_state = 0)

# Escalado de variables
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
