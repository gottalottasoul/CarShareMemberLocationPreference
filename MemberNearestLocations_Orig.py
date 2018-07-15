import pandas as pd
import scipy
from scipy import spatial
from math import radians, cos, sin, asin, sqrt


#####User Functions############

#build the kdtree and get a pointer to the array index of closest location

def kdtree(search_points, vec_points):
    #parse out just the lat/longs for our search points
    search_points=search_points[:,[0,1]]
    mytree = scipy.spatial.cKDTree(search_points)
    dist, indexes = mytree.query(vec_points)
    return indexes

#calculate distances between two points
def gethaversine(row):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [row['ADDRESS_LONG'], row['ADDRESS_LAT'], row['pod_long'], row['pod_lat']])

    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    #r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    r = 3956 # Radius of earth in kilometers. Use 3956 for miles
    return c * r

#Simple function for getting PodName so we can apply function to df
def getPodData(x,y):
    poddata = pod_array[x,y]
    return poddata
#####End User Functions############

### load member dataset
member_filename = 'C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/Geo/Member_Locations-Current.txt'
df_member = pd.read_table(member_filename)
df_member=df_member.dropna()
#shouldnt need this with the updated extract
df_member.rename(columns={'LATITUDE': 'ADDRESS_LAT', 'LONGITUDE': 'ADDRESS_LONG'}, inplace=True)


### load locations dataset
pod_filename = 'C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/pod locations/Zip_Locations.txt'
df_pod = pd.read_table(pod_filename)


### convert to arrays for kdtree
member_array = df_member.as_matrix(columns= ['ADDRESS_LAT', 'ADDRESS_LONG'])
pod_array = df_pod.as_matrix(columns= ['LATITUDE', 'LONGITUDE','LOCATION_ID','LOCATION'])

#returns nearest neighbour (pod) for each member as index of array
result = kdtree(pod_array,member_array)

#append it all to the member dataframe
df_member['pod_index'] = pd.Series(result, index=df_member.index)
df_member['pod_lat']=df_member['pod_index'].apply(getPodData,args=(0,))
df_member['pod_long']=df_member['pod_index'].apply(getPodData,args=(1,))
df_member['LOCATION_ID']=df_member['pod_index'].apply(getPodData,args=(2,))
df_member['LOCATION_NAME']=df_member['pod_index'].apply(getPodData,args=(3,))
df_member['pod_dist']=df_member.apply(gethaversine,axis=1)

pod_dist_lb=df_member['pod_dist'].quantile(.25)
pod_dist_ub=df_member['pod_dist'].quantile(.75)
pod_dist_intraquad=(pod_dist_ub-pod_dist_lb)*1.5
pod_dist_ub=pod_dist_ub+pod_dist_intraquad
pod_dist_lb=pod_dist_lb-pod_dist_intraquad

df_member['is_Outlier']=0
df_member['is_Outlier'][(df_member['pod_dist'] > pod_dist_ub) | (df_member['pod_dist'] < pod_dist_lb)] = 1

#write out the results
df_member.to_csv('c:/users/babbenante/documents/my stuff/data/geo/closest_member_pod_locations.txt',sep='\t',index=False)
