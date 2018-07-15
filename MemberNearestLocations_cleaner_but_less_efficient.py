import pandas as pd
import scipy
from scipy import spatial
from math import radians, cos, sin, asin, sqrt
from operator import itemgetter
from array import array

#####User Functions############

#build the kdtree and get a pointer to the array index of closest location

def kdtree(search_points, vec_points):
#parse out just the lat/longs for our search points
    search_points=search_points[:,[0,1]]
    mytree = scipy.spatial.cKDTree(search_points)
    dist, indexes = mytree.query(vec_points)
    return indexes


#calculate distances between two points abd get pod details
def getPodInfo(row):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    
    latj, lonj, podname = pod_array[row['loc_index']]

    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [row['ADDRESS_LONG'], row['ADDRESS_LAT'], lonj, latj])

    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    #r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    r = 3956 # Radius of earth in kilometers. Use 3956 for miles
    return pd.Series([latj, lonj, podname,c*r], index=['pod_lat', 'pod_long', 'pod_name','pod_distance'])

#####End User Functions############

### load member dataset
member_filename = 'c:/users/babbenante/documents/my stuff/data/geo/Member_Locations.txt'
df_member = pd.read_table(member_filename)
df_member=df_member.dropna()
df_member=df_member[0:50000]

### load locations dataset
pod_filename = 'c:/users/babbenante/documents/my stuff/data/pod locations/Zip_Locations.txt'
df_pod = pd.read_table(pod_filename)


### convert to arrays for kdtree
member_array = df_member.as_matrix(columns= ['ADDRESS_LAT', 'ADDRESS_LONG'])
pod_array = df_pod.as_matrix(columns= ['LATITUDE', 'LONGITUDE','LOCATION'])

#returns nearest neighbour (pod) for each member as index of array
result = kdtree(pod_array,member_array)

#append closest location pointer to the member dataframe
df_member['loc_index'] = pd.Series(result, index=df_member.index)
#append it all to the member dataframe
df_member=pd.concat([df_member, df_member.apply(getPodInfo,axis=1)], axis=1)

#write out the results
df_member.to_csv('c:/users/babbenante/downloads/blake.csv',index='FALSE')



import cProfile

cProfile.run('df_member.apply(getPodInfo,axis=1)')
