# -*- coding: utf-8 -*-
"""
Created on Thu May  7 14:36:08 2020

@author: awitt
"""

#install package geopy
#pip install geopy

#import geocoder
import pandas as pd
from geopy.geocoders import Nominatim

geolocator = Nominatim(user_agent="WQCD GIS")

#test case
location = geolocator.geocode("13088 Road 23, Colorado" )
print((location.latitude, location.longitude))


#import pandas for loading excel data, store as list in mylist
import pandas as pd
df = pd.read_excel(r"C:\Users\awitt\Downloads\geocode.csv") # can also index sheet by name or fetch all sheets
mylist = df['address3'].tolist()

#iterate through mylist and geolocate addresses, otherwise give an error message
for row in mylist:
    location = geolocator.geocode(row, timeout = 99999)
    if location == None:
        print("error, ", row)
    else:
        print(str(location.latitude) + ", " + str(location.longitude) + ", " + row)
print("task completed")

#console print out is comma seperated for csv
#but make sure your console can store 5000 entries (at least for this dataset)


#now I need to fix the errors
import pandas as pd
df = pd.read_excel(r"C:\Users\awitt\Desktop\python\coords_2_latlong_errors.xlsx")

mylist2 = df['address_CO'].tolist()

for row in mylist2:
    location = geolocator.geocode(row, timeout = 99999)
    if location == None:
        print("error, ", row)
    else:
        print(str(location.latitude) + ", " + str(location.longitude) + ", " + row)
print("task completed")



