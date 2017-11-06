# -*- coding: utf-8 -*-
"""
Created on Sat Nov 04 20:51:06 2017

@author: Tianxin.Liu
"""

#call libraries to request pages and parse HTML/CSS to be scraped
import requests
from bs4 import BeautifulSoup

#create two data files and write the headers
f = open('Atl_Hurricanes.txt', 'w')
f.write('year|name|hLink|date|maxWind|minPressure|deaths|damage|landfall' + '\n')

#FOR loop through the years, 2001 to 2017, and grab the hurricane lists from those pages
for y in range(2001,2018):
    year = str(y)
    listPage = 'https://www.wunderground.com/hurricane/at' + year + '.asp'
    page = requests.get(listPage)
    soup = BeautifulSoup(page.text, 'html5lib')
    hurrTable = soup.find('table', id='stormList')
    hurricanes = hurrTable.findAll('tr')[1:]
    
#once we have the hurricanes, grab the table information from the year pages, iterating through rows in the table    
    for h in hurricanes:
        name = h.find('a').string
        hLink = h.find('a')['href']
        date = h.findAll('td')[1].string
        maxWind = h.findAll('td')[2].string.replace('\n','').strip()
        minPressure = h.findAll('td')[3].string.replace('\n','').strip()
        deaths = h.findAll('td')[4].string.replace('\n','').strip()
        damage = h.findAll('td')[5].string.replace('\n','').strip()
        landfall = h.findAll('td')[6].string.replace('\n','').strip()
        
        #write the data
        f.write(year + '|' + name + '|' + str(hLink) + '|' + date + '|' + maxWind + '|' + minPressure + '|' + deaths + '|' + damage + '|' + landfall + '\n')
        print(year + ' ' + name)

#close it all up
f.close()
