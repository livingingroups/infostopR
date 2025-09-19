import numpy as np
from infomap import Infomap
import numpy.typing as npt


IntVector = npt.NDArray[np.int_]


def neighbors_to_network(
  node_idx_neighbors: np.ndarray,
  node_idx_distances: np.ndarray | None,
  counts: IntVector,
  weight_exponent: float = 1.0,
  distance_metric: str = "haversine"
) -> tuple[Infomap, list[int], dict[int, int]]:
  """Create an Infomap network from neighbor relationships and distances.

  This function constructs a network suitable for Infomap clustering by adding
  nodes and weighted edges based on neighbor relationships. Nodes with only
  one neighbor are treated as singletons.

  Args:
    node_idx_neighbors: Array of arrays containing neighbor indices for each node.
      Example: array([array([0]), array([1]), array([2]), ...], dtype=object).
    node_idx_distances: Array of arrays containing distances to neighbors for 
      each node. Can be None for unweighted networks.
    counts: Array of observation counts for each node, used for edge weighting.
    weight_exponent: Exponent used for distance-based edge weighting when 
      node_idx_distances is provided. Defaults to 1.0.
    distance_metric: Distance metric to use, either 'haversine' for geographic 
      coordinates or 'euclidean' for Cartesian coordinates. Defaults to "haversine".

  Returns:
    Tuple containing:
      - network: Infomap network object ready for clustering
      - singleton_nodes: List of node indices that have no connections
      - name_map_inverse: Dictionary mapping Infomap indices to original node indices
  """
  # Initiate  two-level Infomap
  network = Infomap("--two-level --silent")

  # Add nodes (and reindex nodes because Infomap wants ranked indices)
  name_map, name_map_inverse = {}, {}
  singleton_nodes = []
  infomap_idx = 0
  for n, neighbors in enumerate(node_idx_neighbors):
    if len(neighbors) > 1:
      network.addNode(infomap_idx)
      name_map_inverse[infomap_idx] = n
      name_map[n] = infomap_idx
      infomap_idx += 1
    else:
      singleton_nodes.append(n)

  if node_idx_distances is None:
    for node, neighbors in enumerate(node_idx_neighbors):
      for neighbor in neighbors[neighbors > node]:
        network.addLink(
          name_map[node],
          name_map[neighbor],
          max(counts[node], counts[neighbor]),
        )
  else:
    for node, (neighbors, distances) in enumerate(zip(node_idx_neighbors, node_idx_distances)):
      for neighbor, distance in zip(neighbors[neighbors > node], distances[neighbors > node]):
        if distance_metric == "haversine":
          distance *= 6371000
        network.addLink(
          name_map[node],
          name_map[neighbor],
          max(counts[node], counts[neighbor]) * distance ** (-weight_exponent),
        )
  
  return network, singleton_nodes, name_map_inverse


def run_network(
  network: Infomap,
  name_map_inverse: dict[int, int]
) -> dict[int, int]:
  if len(name_map_inverse) > 0:
    network.run()
    # Convert to node-community dict format
    partition = dict([(name_map_inverse[infomap_idx], module) for infomap_idx, module in network.modules])
  else:
    partition = {}
  return partition


def assign_labels_to_nodes(
  node_idx_neighbors: np.ndarray,
  partition: dict[int, int],
  singleton_nodes: list[int],
  label_singleton: bool = True
) -> IntVector:
  # Add new labels to each singleton point (stop that was further than r2 from
  # any other point and thus was not represented in the network)
  if label_singleton:
    max_label = max(partition.values(), default=-1)
    partition.update(dict(
      zip(singleton_nodes,
      range(max_label + 1, max_label + 1 + len(singleton_nodes)))
    ))

  # Cast the partition as a vector of labels like `[0, 1, 0, 3, 0, 0, 2, ...]`
  stat_labels = [partition[n] if n in partition else - 1 for n in range(len(node_idx_neighbors))]
  return np.array(stat_labels)


def label_network(
  node_idx_neighbors: np.ndarray,
  node_idx_distances: np.ndarray | None,
  counts: IntVector,
  weight_exponent: float = 1,
  label_singleton: bool = True,
  distance_metric: str = "haversine"
) -> IntVector:
  """Infer infomap clusters from distance matrix and link distance threshold.

  This function creates a network from neighbor relationships and distances,
  then uses the Infomap algorithm to detect community structure and assign
  cluster labels to nodes.

  Args:
    node_idx_neighbors: Array of neighbor indices for each node.
    node_idx_distances: Array of distances to neighbors for each node.
    counts: Array of observation counts for each node.
    weight_exponent: Exponent used for distance-based edge weighting. 
      Defaults to 1.
    label_singleton: If True, give stationary locations that were only 
      visited once their own label. If False, label them as outliers (-1).
      Defaults to True.
    distance_metric: Distance metric to use, either 'haversine' for 
      geographic coordinates or 'euclidean' for Cartesian coordinates.
      Defaults to "haversine".

  Returns:
    Array of cluster labels matching input length. Detected stop locations 
    are labeled from 0 and up, with locations having more observations 
    typically receiving lower indices. If label_singleton=False, coordinates 
    with no neighbors are labeled -1.
  """
  # Infer the partition with infomap. Partiton looks like `{node: community, ...}`
  network, singleton_nodes, name_map_inverse = neighbors_to_network(
    node_idx_neighbors,
    node_idx_distances,
    counts,
    weight_exponent,
    distance_metric
  )

  partition = run_network(network, name_map_inverse)

  # Add new labels to each singleton point (stop that was further than r2 from
  # any other point and thus was not represented in the network)
  if label_singleton:
    max_label = max(partition.values(), default=-1)
    partition.update(dict(
      zip(singleton_nodes,
      range(max_label + 1, max_label + 1 + len(singleton_nodes)))))

  # Cast the partition as a vector of labels like `[0, 1, 0, 3, 0, 0, 2, ...]`
  stat_labels = [partition[n] if n in partition else - 1 for n in range(len(node_idx_neighbors))]
  return np.array(stat_labels)

