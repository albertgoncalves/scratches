#!/usr/bin/env python3

from collections import Counter
from collections import defaultdict
from glob import glob
from math import exp
from math import log
from random import seed
from re import findall
from re import sub

from utils import split_data

# via https://github.com/joelgrus/data-science-from-scratch


def tokenize(message):
    message = message.lower()
    all_words = findall("[a-z0-9']+", message)
    return set(all_words)


def count_words(training_set):
    counts = defaultdict(lambda: [0, 0])
    for message, is_spam in training_set:
        for word in tokenize(message):
            counts[word][0 if is_spam else 1] += 1
    return counts


def word_proba(counts, total_spams, total_non_spams, k=0.5):
    def p(word, spam, non_spam):
        prob_if_spam = (spam + k) / (total_spams + 2 * k)
        prob_if_not_spam = (non_spam + k) / (total_non_spams + 2 * k)
        return (word, prob_if_spam, prob_if_not_spam)

    return [p(w, spam, non_spam) for w, (spam, non_spam) in counts.items()]


def spam_proba(word_probs, message):
    message_words = tokenize(message)
    log_prob_if_spam = 0.0
    log_prob_if_not_spam = 0.0

    for word, prob_if_spam, prob_if_not_spam in word_probs:
        if word in message_words:
            log_prob_if_spam += log(prob_if_spam)
            log_prob_if_not_spam += log(prob_if_not_spam)
        else:
            log_prob_if_spam += log(1.0 - prob_if_spam)
            log_prob_if_not_spam += log(1.0 - prob_if_not_spam)

    prob_if_spam = exp(log_prob_if_spam)
    prob_if_not_spam = exp(log_prob_if_not_spam)

    return prob_if_spam / (prob_if_spam + prob_if_not_spam)


def p_spam_given_word(word_prob):
    word, prob_if_spam, prob_if_not_spam = word_prob
    return prob_if_spam / (prob_if_spam + prob_if_not_spam)


def train(training_set, k):
    num_spams = len([is_spam for _, is_spam in training_set if is_spam])
    num_non_spams = len(training_set) - num_spams
    wc = count_words(training_set)
    return word_proba(wc, num_spams, num_non_spams, k)


def classify(word_probs, message):
    return spam_proba(word_probs, message)


def load_data(path):
    path = r"{}/*/*".format(path)
    data = []

    for fp in glob(path):
        is_spam = "ham" not in fp

        with open(fp, "r", errors="ignore") as f:
            for line in f:
                if line.startswith("Subject:"):
                    subject = sub(r"^Subject: ", "", line).strip()
                    data.append((subject, is_spam))

    return data


def main():
    seed(0)
    k = 0.5
    data = load_data("data")
    train_data, test_data = split_data(data, 0.75)

    word_probs = train(train_data, k)

    f = lambda subject, is_spam: \
        (subject, is_spam, classify(word_probs, subject))
    classified = [f(subject, is_spam) for subject, is_spam in test_data]

    counts = \
        ((is_spam, spam_proba > 0.5) for _, is_spam, spam_proba in classified)
    counts = Counter(counts)

    words = sorted(word_probs, key=p_spam_given_word)

    print(counts)
    print(words[-5:])
    print(words[:5])


if __name__ == "__main__":
    main()
