import numpy as np
import cpputils
from typing import List, Union
from infostop import utils
import numpy.typing as npt


IntVector = npt.NDArray[np.int_]


def example_data():
  """
  Generates example data for testing the Infostop model.

  This function returns a list of NumPy arrays, where each array represents 
  a sequence of geospatial coordinates (latitude, longitude) and timestamps. 
  Each array corresponds to a different trajectory or path.

  Returns:
    list of np.ndarray: A list containing NumPy arrays. Each array has the 
    shape (n, 3), where n is the number of points in the trajectory. The 
    columns represent:
      - Latitude (float)
      - Longitude (float)
      - Timestamp (int, Unix epoch time)

  Example:
    >>> data = example_data()
    >>> len(data)
    3
    >>> data[0].shape
    (6, 3)
  """
  return [
    np.array([
      [55.75259295, 12.34353885, 1581401760],
      [55.7525908, 12.34353145, 1581402760],
      [55.7525876, 12.3435386, 1581403760],
      [63.40379175, 10.40477095, 1583401760],
      [63.4037841, 10.40480265, 1583402760],
      [63.403787, 10.4047871, 1583403760]
    ]),
    np.array([
      [60.169856, 24.938379, 1581501760],
      [60.169859, 24.938382, 1581502760],
      [60.169862, 24.938385, 1581503760],
      [65.012093, 25.465076, 1583501760],
      [65.012096, 25.465079, 1583502760],
      [65.012099, 25.465082, 1583503760]
    ]),
    np.array([
      [48.856613, 2.352222, 1581601760],
      [48.856616, 2.352225, 1581602760],
      [48.856619, 2.352228, 1581603760],
      [41.902782, 12.496366, 1583601760],
      [41.902785, 12.496369, 1583602760],
      [41.902788, 12.496372, 1583603760]
    ])
  ]


def identify_stops(
  data: Union[np.ndarray, List[np.ndarray]],
  r1: float = 10,
  min_size: int = 2,
  min_staying_time: int = 300,
  max_time_between: int = 86400,
  distance_metric: str = "haversine"
) -> tuple[List[np.ndarray], List[np.ndarray]]:
  """Identify stationary events from mobility traces.

  This function performs the first step of the Infostop algorithm by detecting
  stationary events in location sequences through sequential downsampling. It 
  identifies which points are stationary and stores only the median of each 
  stationarity event.

  Note:
    Currently ONLY works for 2-dimensional spatial data. Data columns 0 and 1 are
    always interpreted as spatial locations while column 2 is interpreted as time.

  Args:
    data: numpy.array (shape (N, 2)/(N, 3)) or list of such numpy.arrays.
      Columns 0 and 1 are reserved for lat and lon. Column 2 is reserved for time 
      (any unit consistent with `min_staying_time` and `max_time_between`). If the 
      input type is a list of arrays, each array is assumed to be the trace of a 
      single user, in which case the obtained stop locations are shared by all users.
    r1: Max distance between time-consecutive points to label them as stationary.
      A point belongs to a stationarity event if it is less than `r1` distance 
      units away from the median of the time-previous collection of stationary points.
      Defaults to 10.
    min_staying_time: The shortest duration that can constitute a stop.
      Defaults to 300.
    max_time_between: The longest duration that can constitute a stop.
      Defaults to 86400.
    min_size: Minimum size of group to consider it stationary. Defaults to 2.
    distance_metric: Either 'haversine' (for geo data) or 'euclidean'.
      Defaults to "haversine".

  Returns:
    A dictionary containing:
      - "stop_events": list of stationary events for each input trace  
      - "event_maps": list of event mappings for each input trace

  Example:
    >>> stops_data = identify_stops(traces, r1=15, min_staying_time=600)
    >>> stop_events = stops_data["stop_events"]
    >>> event_maps = stops_data["event_maps"]
  """
  if isinstance(data, np.ndarray):
    data = [data]
  stop_events = []
  event_maps = []
  for u, coords_u in enumerate(data):
    stop_events_u, event_map_u = cpputils.get_stationary_events(
      coords_u,
      r1,
      min_size,
      min_staying_time,
      max_time_between,
      distance_metric
    )
    stop_events.append(stop_events_u)
    event_maps.append(event_map_u)
  return (stop_events, event_maps)



def downsample(
  stop_events,
  min_spacial_resolution: float = 0
):
  
  stat_coords = [se for se in stop_events if len(se) > 0]
  emsg = "No stop events found. " \
    "Check that `r1`, `min_staying_time` and `min_size` parameters are chosen correctly."
  if (len(stat_coords) == 0):
    raise ValueError(emsg)
  
  stat_coords = np.vstack(stat_coords)
  if min_spacial_resolution > 0:
    stat_coords = np.around(stat_coords / min_spacial_resolution) * min_spacial_resolution

  stat_coords, inverse_indices, counts = np.unique(
    stat_coords, return_inverse=True, return_counts=True, axis=0
  )
  return (stat_coords, inverse_indices, counts)


def find_neighbors(
  stat_coords,
  r2: int | float = 10,
  distance_metric: str = "haversine",
  weighted: bool = False
):
  ball_tree_result = utils.query_neighbors(stat_coords, r2, distance_metric, weighted)
  if weighted:
    node_idx_neighbors, node_idx_distances = ball_tree_result
  else:
    node_idx_neighbors, node_idx_distances = ball_tree_result, None
  return (node_idx_neighbors, node_idx_distances)


def infomap_network(
  node_idx_neighbors,
  node_idx_distances,
  counts,
  weight_exponent: float = 1.0,
  label_singleton: bool = True,
  distance_metric: str = "haversine"
):
  """
  """
  stat_labels = utils.label_network(
    node_idx_neighbors,
    node_idx_distances,
    counts,
    weight_exponent,
    label_singleton,
    distance_metric,
    verbose=False
  )
  return stat_labels


def backtransform(
  stat_labels,
  inverse_indices,
  event_maps,
  stop_events
):
  # (5) Reverse the downsampling in step (2)
  labels = stat_labels[inverse_indices]
  # (6) Reverse the downsampling in step (1)
  new_labels = []
  for j, event_map_u in enumerate(event_maps):
      i0 = sum([len(stop_events[j_]) for j_ in range(j)])
      i1 = sum([len(stop_events[j_]) for j_ in range(j + 1)])
      labels_u = np.hstack([labels[i0:i1], -1])
      new_labels.append(labels_u[event_map_u])
  return new_labels


def identify_sites(
  stop_events: list[np.ndarray] | list[list[float]],
  event_maps: list[list[int]],
  r2: int | float = 10,
  label_singleton: bool = True,
  min_spacial_resolution: float = 0.0,
  distance_metric: str = "haversine",
  weighted: bool = False,
  weight_exponent: float = 1.0
) -> List[IntVector]:
  stat_coords, inverse_indices, counts = downsample(stop_events, min_spacial_resolution)
  node_idx_neighbors, node_idx_distances = find_neighbors(stat_coords, r2, distance_metric, weighted)
  stat_labels = infomap_network(
    node_idx_neighbors, node_idx_distances, counts,
    weight_exponent, label_singleton, distance_metric
  )
  new_labels = backtransform(stat_labels, inverse_indices, event_maps, stop_events)
  return new_labels
