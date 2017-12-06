import numpy as np
import pandas as pd
import mxnet as mx
import os
import sys

def transform_text(text):
    text = text[:FEATURE_LEN][::-1].lower()
    character_hash = pd.DataFrame(np.identity(len(ALPHABET), dtype='bool'), columns=ALPHABET)
    X_split = np.zeros([1, 1, FEATURE_LEN, len(ALPHABET)], dtype='bool')
    for ci, ch in enumerate(list(text)):
        if ch in ALPHABET:
            X_split[0][0][ci] = np.array(character_hash[ch], dtype='bool')
    return mx.nd.array(X_split)


#### Load the model into memory ####
ALPHABET = list("abcdefghijklmnopqrstuvwxyz0123456789-,;.!?:'\"/\\|_@#$%^&*~`+ =<>()[]{}")
FEATURE_LEN = 1014
model = mx.model.FeedForward.load(sys.argv[1], sys.argv[2])

text = "I love Pikatchu Pokemon"
text = transform_text(text)
probs = model.predict(text)
print(probs)
