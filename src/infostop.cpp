/*
 * This code is derived from the Infostop project (https://github.com/ulfaslak/infostop),
 * originally implemented for detecting stationary events in trajectory data.
 * The implementation has been translated and adapted for use with R via the Rcpp interface,
 * enabling efficient clustering of trajectory points based on spatial and temporal criteria.
 * The main function, get_stationary_events, identifies stationary clusters and outputs their
 * coordinates and event mapping, supporting both haversine and euclidean distance metrics.
 * 
 * URL to the orginal source:
 * https://github.com/ulfaslak/infostop/blob/master/cpputils/main.cpp
 *
 * Infostop License:
 *
 * Infostop: Python package for detecting stops in trajectory data.
 * Copyright (c) 2019-2021, Ulf Aslak
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// ----------------
// Regular C++ code
// ----------------

/*
 * Haversine formula: great-circle distance between two points on a sphere,
 * given their latitudes and longitudes in degrees.
 *
 *   a = sin^2(dLat/2) + cos(lat1) * cos(lat2) * sin^2(dLon/2)
 *   d = 2 * R * asin(sqrt(a))
 *
 * Reference:
 * https://www.geeksforgeeks.org/dsa/haversine-formula-to-find-distance-between-two-points-on-a-sphere/
 * Note: unlike the reference (R = 6371 km), this uses R = 6371000 m and
 * therefore returns the distance in meters.
 */
static double haversine(double lat1, double lon1, double lat2, double lon2)
{
  // source: https://bit.ly/2S0OdUs
  // distance between latitudes
  // and longitudes
  double dLat = (lat2 - lat1) * M_PI / 180.0;
  double dLon = (lon2 - lon1) * M_PI / 180.0;

  // convert to radians
  lat1 = (lat1)*M_PI / 180.0;
  lat2 = (lat2)*M_PI / 180.0;

  // apply formulae
  double a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1) * cos(lat2);
  double rad = 6371000;
  double c = 2 * asin(sqrt(std::max(0.0, std::min(1.0, a))));
  return rad * c;
}

static double euclidean(double x1, double y1, double x2, double y2)
{
  return sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
}

double median(std::vector<double> &values)
{
  // the vector comes in pre-sorted
  // if (!std::is_sorted(values.begin(), values.end()))
  // {
  // 	std::cout << "!";
  // }

  // Find the two middle positions (they will be the same if size is odd)
  int i0 = (values.size() - 1) / 2;
  int i1 = values.size() / 2;
  return 0.5 * (values[i0] + values[i1]);
}

void insert_ordered(std::vector<double> &arr, double elem)
{
  // add space
  arr.push_back(elem);

  // find the position for the element
  auto pos = std::upper_bound(arr.begin(), arr.end() - 1, elem);

  // and move the array around it:
  std::move_backward(pos, arr.end() - 1, arr.end());

  // and set the new element:
  *pos = elem;
};

/*

It is more common to use the order longitude, latitude in programming,
since this aligns with the standard (x, y) coordinate pair where
x (horizontal) is longitude and
y (vertical) is latitude.
Infostop uses the order latitude, longitude.
Note:
  We name the input lon, lat in accordance with the infostop naming stop_points_lon, stop_points_lat.

*/
//' @noRd
// [[Rcpp::export(name = ".get_stationary_events_cpp")]]
Rcpp::List get_stationary_events(
  Rcpp::NumericVector lon,
  Rcpp::NumericVector lat,
  Rcpp::NumericVector time,
  double r_C,
  size_t min_size,
  double min_staying_time,
  double max_staying_time,
  std::string distance_metric)
{
  // Get shape of data
  size_t N = lon.length();
  bool no_time_col = time.length() == 0;

  // Output variables
  std::vector<std::vector<double>> stat_coords; // stat_coords (py: list of lists)
  std::vector<int> event_map(N);                // event_map   (py: list)

  // Intermediate variables
  std::vector<double> stop_points_lat;
  std::vector<double> stop_points_lon;
  double ddist;
  double dtime;
  int i0 = 0; // index at which stops begin
  int j = 0;  // index of stat_coords
  int outlier = -1;

  // Prepare distance function. Reject unknown metrics explicitly — falling
  // through to an uninitialised pointer would corrupt the result silently.
  double (*distance_function)(double, double, double, double);
  if (distance_metric == "haversine")
  {
    distance_function = &haversine;
  }
  else if (distance_metric == "euclidean")
  {
    distance_function = &euclidean;
  }
  else
  {
    Rcpp::stop("Unknown distance_metric '%s'. Supported: 'haversine', 'euclidean'.",
               distance_metric);
  }

  if (no_time_col)
  {

    // Cluster with no time information //
    // -------------------------------- //

    // Set current group
    stop_points_lat.push_back(lat(0));
    stop_points_lon.push_back(lon(0));

    // Loop over points
    for (size_t i = 1; i < N; i++)
    {
      // compute distance to median of previous group
      ddist = distance_function(
          lat(i), lon(i),
          median(stop_points_lat), median(stop_points_lon));

      if (ddist <= r_C)
      {
        // append to current group
        insert_ordered(stop_points_lat, lat(i));
        insert_ordered(stop_points_lon, lon(i));
      }
      else
      {
        // test if there are enough points in the stop
        if (i - i0 >= min_size)
        {
          // add previous group median to `stat_coords`,
          stat_coords.push_back(
              {median(stop_points_lat), median(stop_points_lon)});

          // add indices to event_map
          for (size_t idx = i0; idx < i; idx++)
          {
            event_map[idx] = j;
          }

          // increment group index
          j += 1;
        }
        else
        {
          // add indices to event_map
          for (size_t idx = i0; idx < i; idx++)
          {
            event_map[idx] = outlier;
          }
        }

        // clear current groups
        stop_points_lat.clear();
        stop_points_lon.clear();

        // and write current coordinates as new group
        stop_points_lat.push_back(lat(i));
        stop_points_lon.push_back(lon(i));

        // reset i0 index
        i0 = i;
      }
    }
    // append the last group (compact version of what's inside the above for->else)
    if (N - i0 >= min_size)
    {
      stat_coords.push_back({median(stop_points_lat), median(stop_points_lon)});
      for (size_t idx = i0; idx < N; idx++)
        event_map[idx] = j;
    }
    else
    {
      for (size_t idx = i0; idx < N; idx++)
        event_map[idx] = outlier;
    }
  }
  else
  {

    // Cluster WITH time information //
    // ----------------------------- //

    // Set current group
    stop_points_lat.push_back(lat(0));
    stop_points_lon.push_back(lon(0));

    // Loop over points
    for (size_t i = 1; i < N; i++)
    {
      // compute distance to median of previous group
      ddist = distance_function(
          lat(i), lon(i),
          median(stop_points_lat), median(stop_points_lon));
      dtime = time(i) - time(i - 1);

      if (ddist <= r_C && dtime <= max_staying_time)
      {
        // append to current group
        insert_ordered(stop_points_lat, lat(i));
        insert_ordered(stop_points_lon, lon(i));
      }
      else
      {
        // test if there are enough points in the stop and the stop lasts long enough
        if (i - i0 >= min_size && time(i - 1) - time(i0) >= min_staying_time)
        {
          // add previous group median to `stat_coords`,
          stat_coords.push_back(
              {median(stop_points_lat), median(stop_points_lon)});

          // add indices to event_map
          for (size_t idx = i0; idx < i; idx++)
          {
            event_map[idx] = j;
          }

          // increment group index
          j += 1;
        }
        else
        {
          // add indices to event_map
          for (size_t idx = i0; idx < i; idx++)
          {
            event_map[idx] = outlier;
          }
        }

        // clear current groups
        stop_points_lat.clear();
        stop_points_lon.clear();

        // and write current coordinates as new group
        stop_points_lat.push_back(lat(i));
        stop_points_lon.push_back(lon(i));

        // reset i0 index
        i0 = i;
      }
    }

    // append the last group
    if (N - i0 >= min_size && time(N - 1) - time(i0) >= min_staying_time)
    {
      stat_coords.push_back({median(stop_points_lat), median(stop_points_lon)});
      for (size_t idx = i0; idx < N; idx++)
        event_map[idx] = j;
    }
    else
    {
      for (size_t idx = i0; idx < N; idx++)
        event_map[idx] = outlier;
    }
  }

  size_t n_coords = stat_coords.size();
  Rcpp::NumericMatrix stat_coords_matrix(n_coords, 2);

  // We switch here the order of the medians to be consistent with the common order
  // longitude, latitude.
  for (size_t i = 0; i < n_coords; i++) {
    stat_coords_matrix(i, 0) = stat_coords[i][1];  // longitude
    stat_coords_matrix(i, 1) = stat_coords[i][0];  // latitude
  }
  Rcpp::CharacterVector colnames = {"x", "y"};
  stat_coords_matrix.attr("dimnames") = Rcpp::List::create(R_NilValue, colnames);

  return Rcpp::List::create(
    // stat_coords: Medians of the coordinates of the stop locations.
    Rcpp::Named("stop_events") = stat_coords_matrix,
    // event_map: Ids of the stop locations.
    Rcpp::Named("event_map") = event_map
  );
}
