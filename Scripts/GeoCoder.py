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

    geolocator = ArcGIS() #some parameters here
    Fails = []
    for index, row in Indexed_Targets.iterrows():
		Address = row['Address']
		Result = geolocator.geocode(Address, timeout = 10) # wait 100 seconds for the geocoding service to respond before raising a geopy.exc.GeocoderTimedOut exception.
		print "Successes", index ,"Failures: ", len(Fails)
		if Result == None:
			Result = geolocator.geocode(Address[:-7])
			if Result == None:
				Fails.append[Address]	
			else:
				Indexed_Targets.set_value(index, 'Lat', Result.latitude)
				Indexed_Targets.set_value(index, 'Long', Result.longitude)
		else:
			Indexed_Targets.set_value(index, 'Lat', Result.latitude)
			Indexed_Targets.set_value(index, 'Long', Result.longitude)
		for address in Fails:	
			print address
    Indexed_Targets.to_csv(filename[:-4]+"_RESULTS.csv")

if __name__ == '__main__': # special variable _name_ = _main_ when this .py file is the primary (or main) script to be run. http://stackoverflow.com/questions/419163/what-does-if-name-main-do
    main('/users/brandondey/desktop/MKE Parking Ticket Proj', 'top ten addr test.csv') 
  
    #the rest 6000 + addr.csv
main("/users/brandondey/desktop/MKE Parking Ticket Proj",'top ten addr test.csv')