# -*- coding: utf-8 -*-
"""
Created on Thu Nov 02 13:50:30 2017

@author: Tianxin.Liu
"""

import urllib2
from bs4 import BeautifulSoup

# Create/open a file called wunder.txt (which will be a comma-delimited file)
f = open('wunder-data.txt', 'w')

location = {'KLAX','KORD','KDFW','KJFK','KDEN',
            'KSEA','KMIA','KMCO','KMSP','KBOS'
            ,'KDTW'}
    
# Iterate through months and day
for l in location:
    for y in range(2001,2018):
        for m in range(1, 13):
            for d in range(1, 32):
    
                # Check if already gone through month
                if (m == 2 and d > 28):
                    break
                elif (m in [4, 6, 9, 11] and d > 30):
                    break
    
                # Open wunderground.com url
                url = "http://www.wunderground.com/history/airport/" + str(l) + "/"+ str(y)+ "/" + str(m) + "/" + str(d) + "/DailyHistory.html"
                page = urllib2.urlopen(url)
    
                # Get temperature from page
                soup = BeautifulSoup(page,"lxml")
                dayTemp = soup.findAll(attrs={"class":"wx-value"})[0].contents[0].string
                maxTemp = soup.findAll(attrs={"class":"wx-value"})[2].contents[0].string
                minTemp = soup.findAll(attrs={"class":"wx-value"})[5].contents[0].string

                # Format month for timestamp
                if len(str(m)) < 2:
                    mStamp = '0' + str(m)
                else:
                    mStamp = str(m)
    
                # Format day for timestamp
                if len(str(d)) < 2:
                    dStamp = '0' + str(d)
                else:
                    dStamp = str(d)
    
                # Build timestamp
                timestamp = str(y) + mStamp + dStamp
                
                # Write timestamp and temperature to file
                f.write(str(l) + ',' + timestamp + ',' + dayTemp + ',' + maxTemp + ',' + minTemp + '\n')
    
# Done getting data! Close file.
f.close()
