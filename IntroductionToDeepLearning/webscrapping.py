# -*- coding: utf-8 -*-
"""
Created on Sun Oct 23 14:13:51 2016

@author: celian
"""

from bs4 import BeautifulSoup
from datetime import datetime
from selenium.common.exceptions import NoSuchElementException
from selenium import webdriver
import time
import json

PHANTOMJS_PATH="/home/celian/scrapping_phantomjs/phantomjs"
browser = webdriver.PhantomJS(PHANTOMJS_PATH)

################""""""" MONSTER
browser.get("http://www.monster.fr/")
keyWordsElem = browser.find_element_by_id('q1')
keyWordsElem.send_keys('data')
keyWordsElem.submit()
txt = browser.page_source
soup = BeautifulSoup(txt, "html.parser")

script = soup.find("script").get_text()
soup.find_all('article',attrs= {u'class':u'js_result_row'})
#PlaceElem = browser.find_element_by_id('where1')
#soup = BeautifulSoup(browser.page_source, "html.parser")

######################" APEC
today = datetime.now()

browser.get("https://www.apec.fr/")
keyWordsElem = browser.find_element_by_id('keywords')
keyWordsElem.send_keys('')
keyWordsElem.submit()
time.sleep(10)
txt = browser.page_source
soup = BeautifulSoup(txt, "html.parser")


temp=[]
base_url="https://cadres.apec.fr/"


page_number = 1
while True:
    for offre in soup.find_all('div',attrs= {u'class':u'offre-result'}):
        date_offre=offre.find('p',attrs={u'class':u'date'}).find('span').text
        date_formated=datetime.strptime(date_offre, "%d/%m/%Y")
        if((today - date_formated).days<2):
            url_offre=offre.find('h2',attrs={u'class':u'titreOffreH2'}).find('a').get('href')[1:]
            url_offre="https://cadres.apec.fr/"+url_offre
            temp.append(url_offre)  
      
    try:
        link = browser.find_element_by_link_text("Suiv.")
    except NoSuchElementException:
        break
    link.click()
    print browser.current_url
    page_number += 1
    #browser.page_source



################################""" PAGE WEBSCRAPPING
cmpt=1
for current_offer in temp:
    print("-------------------- ANNONCE N°"+str(cmpt)+"---------------------")
    url=str(current_offer)
    browser.get(url)
    time.sleep(3)
    txt = browser.page_source
    soup = BeautifulSoup(txt, "html.parser")
    
    data={}
    data['titre']=soup.find("h1").text.encode('utf-8')
    
    for lab in soup.find_all("label"):
        if(lab.text.encode('utf-8')=="Référence apec :"):
            data['apecRef']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Référence société :"):
            data['refSoc']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Date de publication :"): 
            data['datePublication']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Date d'actualisation :"):
            data['dateUpdate']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Société :") :
            data['society']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Nombre de postes :"):
            data['nbPoste']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Statut :"):
            data['statut']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Lieu :"):  
            data['place']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Zone de déplacement :"): 
            data['deplZone']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Salaire :"):    
            data['salary']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Expérience :"):   
            data['exp']=lab.parent.find("strong").text.encode('utf-8')
        if(lab.text.encode('utf-8')=="Dossier suivi par :"):   
            data['suiviDoss']=lab.parent.find("strong").text.encode('utf-8')
    
    data['content']=soup.find("div",attrs={u'class':u'ng-binding'}).text.encode('utf-8')
    json_data = json.dumps(data)
    
    import io
    with io.open('/home/celian/scrapping_phantomjs/data/APEC'+data['apecRef']+'.txt', 'w', encoding='utf-8') as f:
      f.write(unicode(json.dumps(data)))
    cmpt=cmpt+1
