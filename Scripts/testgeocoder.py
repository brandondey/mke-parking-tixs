#from: http://stackoverflow.com/questions/31252863/geocoding-using-geopy-and-python
def main(path, filename):
    import geopy
    from geopy.geocoders import ArcGIS
    import pandas as pd
    import numpy as np
    from geopy.exc import GeocoderTimedOut

    
    Target_Addresses = pd.read_csv(path+'//'+filename) #Read csv into Dataframe. '//' first	 backslash escapes / so we get path/filename.
    Target_Addresses['Lat'] = np.nan
    Target_Addresses['Long'] = np.nan # I think these lines just replace all the missing lat/long coordinates with nan > not a number
    Indexed_Targets = Target_Addresses.set_index('UniqueID')
    
    Failed_Addresses = pd.read_csv(path+'//'+filename)
    Failed_Addresses['Failed'] = ''
    Indexed_Failed = Failed_Addresses.set_index('UniqueID')
    
    df = pd.read_csv('All Failed.csv')
    UniqueIDs = df.UniqueID
    TotalAddressesinFile = max(UniqueIDs)

    geolocator = ArcGIS() 
    Fails = []
    for index, row in Indexed_Targets.iterrows():
	try:	  
		Address = row['Address']
		Result = geolocator.geocode(Address,timeout = 20) # wait 20 seconds for the geocoding service to respond before raising a geopy.exc.GeocoderTimedOut exception.
		if Result == None:
			Result = geolocator.geocode(Address[:-7])
			if Result == None:
				Fails.append(Address)
			else:
				Indexed_Targets.set_value(index, 'Lat', Result.latitude)
				Indexed_Targets.set_value(index, 'Long', Result.longitude)
				Indexed_Targets.to_csv(filename[:-4]+"_RESULTS.csv")
		else:
			Indexed_Targets.set_value(index, 'Lat', Result.latitude)
			Indexed_Targets.set_value(index, 'Long', Result.longitude)
			Indexed_Targets.to_csv(filename[:-4]+"_RESULTS.csv")
		if index %2 ==0 : #only print it if even to save space in output...
			print "Successes:", index," Failures:", len(Fails),'     ---> ', (float(index)/TotalAddressesinFile)*100 ,"% Completed."
		else:
			""
	except GeocoderTimedOut as e: #if address times out, drop it into a separate csv _FAILS.csv and keep chugging...
		Fails.append(Address)#print("Error: geocode failed on input %s with message %s"%(Address, e.message))
		
		Indexed_Failed.set_value(index,'Failed', Address)
		Indexed_Failed.to_csv(filename[:-4]+"_FAILS.csv")
	except Exception, e:
    		print type(e)
   		print e  
	

# if __name__ == '__main__': # special variable _name_ = _main_ when this .py file is the primary (or main) script to be run. #http://stackoverflow.com/questions/419163/what-does-if-name-main-do
#     main('/users/brandondey/desktop/MKE Parking Ticket Proj', '78128 plus addr.csv') 
  
    #top ten addr test.csv
    #top 1000 addr.csv
    #8715 plus.csv
    #12723 plus.csv
    #78128 plus addr.csv')
main("/users/brandondey/desktop/MKE Parking Ticket Proj",'All Failed.csv')



