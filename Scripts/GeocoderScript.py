def main(path, filename):
    import geopy
    from geopy.geocoders import ArcGIS
    import pandas as pd
    import numpy as np
    from geopy.exc import GeocoderTimedOut

    
    Target_Addresses = pd.read_csv(path+'//'+filename) #Read csv into Dataframe. '//' first	 backslash escapes / so we get path/filename.
    Target_Addresses['Lat'] = np.nan
    Target_Addresses['Long'] = np.nan 
    Indexed_Targets = Target_Addresses.set_index('Location')
    
   #  Failed_Addresses = pd.read_csv(path+'//'+filename)
#     Failed_Addresses['Failed'] = ''
#     Indexed_Failed = Failed_Addresses.set_index('Location')
#     
#     df = pd.read_csv('Locations All Failed.csv')
#     UniqueIDs = df.UniqueID
#     TotalAddressesinFile = max(UniqueIDs)

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
# 		Indexed_Failed.to_csv(filename[:-4]+"_FAILS.csv")
	except Exception, e:
    		print type(e)
   		print e  
	
main("/users/brandondey/downloads",'West Bend My Neighborhood.csv')