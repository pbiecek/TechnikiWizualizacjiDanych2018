# -*- coding: utf-8 -*-

""" Use DeepMoji to score texts for emoji distribution.

The resulting emoji ids (0-63) correspond to the mapping
in emoji_overview.png file at the root of the DeepMoji repo.

Writes the result to a csv file.
"""
from __future__ import print_function, division
import example_helper
import json
import csv
import numpy as np
from deepmoji.sentence_tokenizer import SentenceTokenizer
from deepmoji.model_def import deepmoji_emojis
from deepmoji.global_variables import PRETRAINED_PATH, VOCAB_PATH
import srt
import codecs
import argparse
import os

import sys
reload(sys)
sys.setdefaultencoding('utf8')

parser = argparse.ArgumentParser(description='Score movie script emojis')
parser.add_argument('input', metavar='i', type=str,
                           help='input file path')

args = parser.parse_args()

with codecs.open(args.input, encoding='utf-8') as script_file:
    script = script_file.read()
    script = unicode(script.encode('utf-8').strip())

lines = script.split("<b>")
lines = [line.strip() for line in lines if len(line.strip()) > 0]

#print(lines)
       
OUTPUT_PATH = os.path.splitext(args.input)[0] + '.csv'

def top_elements(array, k):
    ind = np.argpartition(array, -k)[-k:]
    return ind[np.argsort(array[ind])][::-1]


maxlen = 30
batch_size = 32

print('Tokenizing using dictionary from {}'.format(VOCAB_PATH))
with open(VOCAB_PATH, 'r') as f:
    vocabulary = json.load(f)
st = SentenceTokenizer(vocabulary, maxlen)
tokenized, _, _ = st.tokenize_sentences(lines)

print('Loading model from {}.'.format(PRETRAINED_PATH))
model = deepmoji_emojis(maxlen, PRETRAINED_PATH)
model.summary()

print('Running predictions.')
prob = model.predict(tokenized)

# Find top emojis for each sentence. Emoji ids (0-63)
# correspond to the mapping in emoji_overview.png
# at the root of the DeepMoji repo.
print('Writing results to {}'.format(OUTPUT_PATH))
scores = []
for i, t in enumerate(lines):
    t_tokens = tokenized[i]
    t_score = [t]
    t_prob = prob[i]
    ind_top = top_elements(t_prob, 5)
    t_score.append(sum(t_prob[ind_top]))
    t_score.extend(ind_top)
    t_score.extend([t_prob[ind] for ind in ind_top])
    scores.append(t_score)
#    print(t_score)

with open(OUTPUT_PATH, 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=';', lineterminator='\n')
    writer.writerow(['Text', 'Top5%',
                     'Emoji_1', 'Emoji_2', 'Emoji_3', 'Emoji_4', 'Emoji_5',
                     'Pct_1', 'Pct_2', 'Pct_3', 'Pct_4', 'Pct_5'])
    for i, row in enumerate(scores):
        try:
            writer.writerow(row)
        except Exception as e:
            print("Exception at row {}!".format(e))
