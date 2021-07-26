# -*- coding: utf-8 -*-
"""
Created on Sat Feb  9 14:05:24 2019

@author: Celian
"""
import requests
#### TEST GOOGLE SEARCH API
google_cs_key='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

import json

from googleapiclient.discovery import build
import pprint
service = build("customsearch", "v1",
            developerKey=google_cs_key)

keywords_1=["bonheur","joie","heureux","bien-être","rire","sérénité","prospérité","comblé","content",
          "joyeux","satisfait","épanoui","réjoui","fier","réussite","aggréable"]
kw1=["satisfait","épanoui","réjoui","fier","réussite"]
keywords_0=["triste","malheureux","mal-être","pleurer","désastre","déprimer","regretable",
            "rage","mélancolie","pénible","morose","chagrin"]
sentiment={"positif":kw1,"negatif":keywords_0}
country=["FR","BJ"]

for s in sentiment:
    s="negatif"
    for k in sentiment[s]:
        res = service.cse().list(      
          q=k,
          cx='008563858261602570784:fgsqw7_3y3o',
          lr="lang_fr",
          sort="date:r:20150101:20151231"
        ).execute()
        print("HEY >"+k)
        with open('C:/Users/Celian/Desktop/BienEtreSubjectif/Data/CSE/2015/FR_'+s+'_'+k+'.json', 'w') as fp:
            json.dump(res, fp)

