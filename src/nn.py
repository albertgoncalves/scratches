#!/usr/bin/env python3

from os import environ

from matplotlib.pyplot import close, savefig, subplots
from numpy import arange, array, dot, dstack, exp, max, mean, min, meshgrid, \
    round, std, sum, zeros
from numpy.random import rand, seed
from scipy.special import expit
from sklearn.datasets import make_blobs, make_circles, make_moons


def sigmoid(x, deriv=False):
    y = expit(x)
    if deriv:
        return y * (1 - y)
    return y


def forward_prop(x, weights_a, weights_b):
    layer_a = sigmoid(dot(x, weights_a))
    model = sigmoid(dot(layer_a, weights_b))
    return (layer_a, model)


def back_prop(x, y, weights_a, weights_b, layer_a, model):
    m = 2 * (y - model) * sigmoid(model, deriv=True)
    d_weights_b = dot(layer_a.T, m)
    d_weights_a = dot(x.T, dot(m, weights_b.T) * sigmoid(layer_a, deriv=True))
    weights_a += d_weights_a
    weights_b += d_weights_b
    return (weights_a, weights_b)


def train(x, y, k, n):
    weights_a = rand(x.shape[1], k)
    weights_b = rand(k, 1)
    for _ in range(n):
        (weights_a, weights_b) = \
            back_prop( x
                     , y
                     , weights_a
                     , weights_b
                     , *forward_prop(x, weights_a, weights_b)
                     )
    return (weights_a, weights_b)


def grid(x, y, k):
    min_x = min(x)
    max_x = max(x)
    min_y = min(x)
    max_y = max(x)
    xs = arange(min(x), max(x), k)
    ys = arange(min(y), max(y), k)
    return dstack(meshgrid(xs, ys)).reshape(-1, 2)


def plot(x, y, z, X, Y, Z):
    _, ax = subplots()
    kwargs = {"cmap": "bwr"}
    ax.tricontourf(X, Y, Z, alpha=0.75, **kwargs)
    ax.scatter(x, y, c=z, edgecolor="w", **kwargs)
    savefig("{}/pngs/plot.png".format(environ["WD"]))
    close()


def predict(x, y, X, k, n):
    y = y.reshape(-1, 1)
    _, Y = forward_prop(X, *train(x, y, k, n))
    return Y.reshape(1, -1)[0]


def main():
    seed(2)
    dataset = \
        { "blobs": make_blobs(n_features=2, centers=2, cluster_std=4)
        , "circles": make_circles(noise=0.2, factor=0.5)
        , "moons": make_moons(noise=0.3)
        }
    x, y = dataset["blobs"]
    X = grid(x.T[0], x.T[1], 0.25)
    Y = predict(x, y, X, 3, 1000)
    plot( x.T[0]
        , x.T[1]
        , y
        , X.T[0]
        , X.T[1]
        , Y
        )


if __name__ == "__main__":
    main()
