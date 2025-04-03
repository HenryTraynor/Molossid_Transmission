# Molossid_Transmission

## Scripts

### 'cluster_map.R'

'cluster_map.R' stores 'clusterMap', which creates a map of roosts on the unit square given a number of roosts and roost clusters desired. The center of each cluster is chosen randomly, and a roost is placed there. The remaining roosts needed are randomly sorted into one of these clusters. Each non-central roost's location is determined by a 'truncnorm' result with x,y mean from the cluster center. The standard deviation of each cluster is prescribed.
