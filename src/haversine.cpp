#include <Rcpp.h>
using namespace Rcpp;

/**
 * @brief Calculates the great-circle distance between two points on a sphere using the Haversine formula.
 *
 * This function computes the shortest distance (the "as-the-crow-flies" or great-circle distance) between two points
 * specified by their longitude and latitude, assuming a spherical Earth. The Haversine formula is robust for most
 * practical purposes and is especially useful for navigation and geospatial analysis.
 *
 * The formula accounts for the Earth's curvature but ignores ellipsoidal effects. For higher accuracy, especially
 * over long distances or near the poles, consider using more advanced methods such as the Vincenty formula.
 *
 * @param lon1 Longitude of the first point in decimal degrees.
 * @param lat1 Latitude of the first point in decimal degrees.
 * @param lon2 Longitude of the second point in decimal degrees.
 * @param lat2 Latitude of the second point in decimal degrees.
 * @param radius Radius of the sphere (default is 6378137.0 meters, the WGS84 mean Earth radius).
 * @return The distance between the two points in the same units as the radius parameter (default is meters).
 *
 * @details
 * The Haversine formula was popularized by R.W. Sinnott in 1984 ("Virtues of the Haversine", Sky and Telescope, vol 68, no 2).
 * It is numerically stable for small distances and avoids errors that can occur with the spherical law of cosines.
 * For antipodal points (opposite sides of the sphere), the formula ensures the result does not exceed the maximum possible distance.
 *
 * Example usage:
 *   double d = dist_haversine_double(lon1, lat1, lon2, lat2);
 * 
 * Note:
 *   This is a reimplementation from the distHaversine function in the geosphere R package.
 *
 * References:
 *   - https://cran.r-project.org/package=geosphere <GPL-3>
 *   - Sinnott, R.W. (1984). "Virtues of the Haversine". Sky and Telescope. 68 (2): 159.
 *   - https://en.wikipedia.org/wiki/Haversine_formula
 *   - http://www.movable-type.co.uk/scripts/latlong.html
 */
// [[Rcpp::export]]
double dist_haversine_double(
    double lon1,
    double lat1,
    double lon2,
    double lat2,
    double radius = 6378137.0
) {
    // Convert degrees to radians
    const double toRad = M_PI / 180.0;
    double lat1_rad = lat1 * toRad;
    double lat2_rad = lat2 * toRad;
    double lon1_rad = lon1 * toRad;
    double lon2_rad = lon2 * toRad;
    
    // Calculate differences
    double dLat = lat2_rad - lat1_rad;
    double dLon = lon2_rad - lon1_rad;
    
    // Haversine formula
    // The max / min is so we don't fall out of the domain due to numerical issues.
    double a = std::max(0.0, std::min(1.0,
        sin(dLat / 2) * sin(dLat / 2) +
        cos(lat1_rad) * cos(lat2_rad) * sin(dLon / 2) * sin(dLon / 2)
    ));
    
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    double dist = radius * c;
    
    return dist;
}


/**
 * @brief Vectorized version of the Haversine distance calculation for multiple point pairs.
 *
 * This function computes the great-circle distances between multiple pairs of points
 * using the Haversine formula. It can handle vectors of coordinates efficiently.
 *
 * @param lon1 Vector of longitudes for the first points in decimal degrees.
 * @param lat1 Vector of latitudes for the first points in decimal degrees.
 * @param lon2 Vector of longitudes for the second points in decimal degrees.
 * @param lat2 Vector of latitudes for the second points in decimal degrees.
 * @param radius Radius of the sphere (default is 6378137.0 meters, the WGS84 mean Earth radius).
 * @return A numeric vector of distances between the point pairs in the same units as the radius parameter.
 *
 * @details
 * All input vectors must have the same length. The function calculates the distance
 * between lon1[i], lat1[i] and lon2[i], lat2[i] for each index i.
 *
 * Example usage:
 *   NumericVector distances = dist_haversine(lon1, lat1, lon2, lat2);
 */
// [[Rcpp::export]]
NumericVector dist_haversine_cpp(
    NumericVector lon1,
    NumericVector lat1,
    NumericVector lon2,
    NumericVector lat2,
    double radius = 6378137.0
) {
    int n = lon1.size();
    
    // Check that all vectors have the same length
    if (lat1.size() != n || lon2.size() != n || lat2.size() != n) {
        Rcpp::stop("All input vectors must have the same length!");
    }
    
    NumericVector distances(n);
    for (int i = 0; i < n; i++) {
        distances[i] = dist_haversine_double(lon1[i], lat1[i], lon2[i], lat2[i], radius);
    }
    
    return distances;
}
