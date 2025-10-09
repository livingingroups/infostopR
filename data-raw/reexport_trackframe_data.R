# The purpose of this file is to freeze external (to infostop)
# data files used for testing in infostop.
# Prevents changes to trackframe data from breaking infostop tests.
save(
  tf_path_matrix = 'path_matrix',
  tf_path_trackframe = 'path_trackframe',
  tf_path_move2 = 'path_move2',
  tf_path_sftrack = 'path_sftrack',

  tf_paths_data_frame = 'paths_data_frame',
  tf_paths_trackframe = 'paths_trackframe',
  tf_paths_move2 = 'paths_move2',
  tf_paths_sftrack = 'paths_sftrack',
  envir = as.environment('package:trackframe'),
  file = 'inst/tinytest/data/tf_paths.RData'
)
