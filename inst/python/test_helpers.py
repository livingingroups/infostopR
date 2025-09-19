"""
Test file to be used with pytest.

Example:
  #sh> pytest .
"""
import numpy as np
from infostop import Infostop
from trackdata import data
from infostop_helper import (
  identify_stops,
  downsample,
  find_neighbors,
  backtransform,
  infomap_network
)
from infomap_helper import neighbors_to_network, run_network, assign_labels_to_nodes
from functools import singledispatch


@singledispatch
def assert_equal(x, y):
  assert x == y

@assert_equal.register
def _assert_equal_ndarray(x: np.ndarray, y: np.ndarray):
  assert np.array_equal(x, y)

@assert_equal.register
def _assert_equal_ndarray(x: list, y: list):
  assert len(x) == len(y)
  for xi, yi in zip(x, y):
    assert_equal(xi, yi)


def test_infostop_helper():
  stop_events, event_maps = identify_stops(data)
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
  ref_labels = model.fit_predict(data)
  assert_equal(new_labels, ref_labels)


def test_infomap_helper():
  stop_events, event_maps = identify_stops(data)
  stat_coords, inverse_indices, counts = downsample(stop_events)
  node_idx_neighbors, node_idx_distances = find_neighbors(stat_coords)
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
  model = Infostop()
  ref_labels = model.fit_predict(data)
  assert_equal(new_labels, ref_labels)
