import numpy as np
import cpputils
from typing import List, Union
from infostop import utils, Infostop, SpatialInfomap
from infostop.infostop import Infostop2
from trackdata import data
from pprint import pprint
from multi_steps import *
from infomap_helper import neighbors_to_network, run_network, assign_labels_to_nodes


stop_events, event_maps = identify_stops(data)
stop_events[1]
data[1]
event_maps
stat_coords, inverse_indices, counts = downsample(stop_events)
node_idx_neighbors, node_idx_distances = find_neighbors(stat_coords)
stat_labels = infomap_network(
  node_idx_neighbors,
  node_idx_distances,
  counts
)
new_labels = backtransform(
  stat_labels,
  inverse_indices,
  event_maps,
  stop_events
)


model = Infostop()
labels_1 = model.fit_predict(data)
labels_2 = model.fit_predict(data)
all(labels_1[2] == labels_2[2])
len(new_labels) == len(labels_2)
all(new_labels[0] == labels_2[0])
all(new_labels[1] == labels_2[1])
all(new_labels[2] == labels_2[2])


model = Infostop2()
labels_3 = model.fit_predict(data)
all(labels_2[2] == labels_3[2])
# pprint(dir(model))

len(stop_events) == len(model.stop_events)
np.max(abs(np.array(stop_events[0]) - np.array(model.stop_events[0])))
np.max(abs(np.array(stop_events[1]) - np.array(model.stop_events[1])))
np.max(abs(np.array(stop_events[2]) - np.array(model.stop_events[2])))

np.all(stat_coords == model._stat_coords)
np.all(inverse_indices == model.inverse_indices)
np.all(counts == model._counts)

node_idx_neighbors
model.node_idx_neighbors[0:5]
node_idx_distances

model._stat_labels
stat_labels


#
# Network
#
network, singleton_nodes, name_map_inverse = neighbors_to_network(
  node_idx_neighbors,
  node_idx_distances,
  counts
)
partition = run_network(network, name_map_inverse)
stat_labels = assign_labels_to_nodes(
  node_idx_neighbors,
  partition,
  singleton_nodes,
  label_singleton=True
)
new_labels = backtransform(
  stat_labels,
  inverse_indices,
  event_maps,
  stop_events
)

len(new_labels) == len(labels_2)
all(new_labels[0] == labels_2[0])
all(new_labels[1] == labels_2[1])
all(new_labels[2] == labels_2[2])


sim = SpatialInfomap()
dat = [np.array(x) for x in stop_events]
dat[0].shape
dat[1].shape
dat[2].shape
sim.fit_predict(dat)

lab1 = identify_sites(stop_events, event_maps)

sevents = [np.array(x) for x in stop_events]
lab2 = identify_sites(sevents, event_maps)


np.all(lab1[0] == lab2[0])
np.all(lab1[1] == lab2[1])
np.all(lab1[2] == lab2[2])
