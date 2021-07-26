# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 23:41:06 2019

@author: Celian
"""


from pytrends.request import TrendReq
import pandas as pd
sentiment={"positif":["bonheur","joie","heureux","bien-être","rire","sérénité","prospérité","comblé","content",
          "joyeux","satisfait","épanoui","réjoui","fier","réussite","aggréable"],"negatif":["triste","malheureux","mal-être","pleurer","désastre","déprimer","regretable",
                "rage","mélancolie","pénible","morose","chagrin"]}
country=["FR","BJ"]
### BENIN BONHEUR
for c in country:
    for s in sentiment.keys():
        df=None
        for k in sentiment[s]:
            print(k)
            pytrends = TrendReq(hl='fr-'+c, tz=1)
            pytrends.build_payload([k], cat=0, timeframe='2017-01-01 2017-12-31', geo=c, gprop='')
            res=pytrends.interest_over_time()
            if df is None :
                df=res
            else:
                
                try:
                    df=pd.concat([df, res[k]], axis=1)
                except: 
                    pass
            print(df.columns)
        df.to_csv('C:/Users/Celian/Desktop/BienEtreSubjectif/Data/GoogleTrends_2017_'+c+'_'+s+'.csv', sep='\t', encoding='utf-8')

### BENIN MALHEUR
pytrends = TrendReq(hl='fr-BJ', tz=1)
pytrends.build_payload(keywords_0, cat=0, timeframe='2016-01-01 2016-12-31', geo='BJ', gprop='')
res=pytrends.get_historical_interest(keywords_0, year_start=2016, month_start=1, day_start=1, hour_start=0, year_end=2016, month_end=12, day_end=31, hour_end=0, cat=0, geo='BJ', gprop='', sleep=0)
res.to_csv('C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Benin_GoogleTrends_2016_Malheur.csv', sep='\t', encoding='utf-8')

### FRANCE BONHEUR
pytrends = TrendReq(hl='fr-FR', tz=1)
pytrends.build_payload(keywords_1, cat=0, timeframe='2016-01-01 2016-12-31', geo='FR', gprop='')
res=pytrends.get_historical_interest(keywords_1, year_start=2016, month_start=1, day_start=1, hour_start=0, year_end=2016, month_end=12, day_end=31, hour_end=0, cat=0, geo='FR', gprop='', sleep=0)
res.to_csv('C:/Users/Celian/Desktop/BienEtreSubjectif/Data/France_GoogleTrends_2016_Bonheur.csv', sep='\t', encoding='utf-8')

### FRANCE MALHEUR
pytrends = TrendReq(hl='fr-FR', tz=1)
pytrends.build_payload(keywords_0, cat=0, timeframe='2016-01-01 2016-12-31', geo='FR', gprop='')
res=pytrends.get_historical_interest(keywords_0, year_start=2016, month_start=1, day_start=1, hour_start=0, year_end=2016, month_end=12, day_end=31, hour_end=0, cat=0, geo='FR', gprop='', sleep=0)
res.to_csv('C:/Users/Celian/Desktop/BienEtreSubjectif/Data/France_GoogleTrends_2016_Malheur.csv', sep='\t', encoding='utf-8')