__author__ = 'vickyzhang'

import pandas as pd
from pandas import DataFrame, Series
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor, BaggingClassifier, GradientBoostingClassifier
from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sknn.mlp import Classifier, Layer, MultiLayerPerceptron
import sklearn
from patsy import dmatrices, dmatrix
from sklearn.svm import SVC
import matplotlib.pyplot as plt
from sknn.mlp import Classifier, Layer, MultiLayerPerceptron, Regressor
from math import pow, sqrt
from statistics import mean
from sklearn.metrics import r2_score

def test():
    boom = pd.read_csv('boommagain.csv')
    boom = boom.drop(['Unnamed: 0', 'Unnamed: 0.1'], axis = 1)
    words = boom[list(boom.columns.values)[8:89]]
    y = boom['Rating']
    x = words
    print(y.shape, type(y))
    print(y[:10])
    print(x.shape, type(x))

    X_train, X_test, y_train, y_test = train_test_split(x, y, test_size = 0.4, random_state = 1)

    X_train = X_train.as_matrix()
    y_train = y_train.as_matrix()
    y_test = y_test.as_matrix()
    X_test = X_test.as_matrix()
    print(y_train[:10])
    print(X_train.shape, type(X_train))
    print(y_train.shape, type(y_train))

    y_test = Series(y_test)
    squared = [pow(x, 2) for x in y_test]
    avg = mean(squared)
    RMSE = sqrt(avg)
    print(RMSE)

    for num_nodes in [20]:
        for epoch in [50]:
            nn = Regressor(
            layers=[
                Layer("Sigmoid", units=num_nodes),
                Layer("Softmax")],
            learning_rate=0.01,
            n_iter=epoch)
            nn.fit(X_train, y_train)
            y_pred = nn.predict(X_test)
            print(y_pred.shape)
            # y_pred = Series(y_pred)
            # y_test = Series(y_test)
            # diff = [y_pred[i] - y_test[i] for i in range(len(y_pred))]
            # squared = [pow(x, 2) for x in diff]
            # avg = mean(squared)
            # RMSE = sqrt(avg)
            # print(RMSE)

            print(metrics.r2_score(y_test, y_pred))


if __name__ == '__main__':
    # load()
    test()