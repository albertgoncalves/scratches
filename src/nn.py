#!/usr/bin/env python3

from numpy import array, dot, exp, zeros
from numpy.random import rand, seed


def sigmoid(x, deriv=False):
    y = 1 / (1 + exp(-x))
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


def main():
    seed(1)
    X = array([ [0, 0, 1]
              , [0, 1, 1]
              , [1, 0, 1]
              , [1, 1, 1]
              ])
    y = array([[0], [1], [1], [0]])
    _, prediction = forward_prop([[0.95, 1.025, 1.005]], *train(X, y, 4, 2500))
    print(prediction)


if __name__ == "__main__":
    main()
