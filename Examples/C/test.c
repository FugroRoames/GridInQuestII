#include <stdio.h>
#include "giq.h"
 
int main(void)
{
coordinates Source;
coordinates Target;
int DatumCode;

puts("Grid InQuest II test...");


// Setup the calling parameter values.
Source.X = -0.12217304764; // Longitude in radians (-7 degrees).
Source.Y = 0.925024503557; // Latitude in radians (53 degrees).
Source.Z = 100; // Altitude in metres.
DatumCode = 13; // Malin Head datum.

// Call coordinate converter: ETRS89 Geodetic, Irish Transverse Mercator, No Revision, ITM/GM15, Source, Target, Malin Head datum. 
if (ConvertCoordinates(4937, 2157, 0, 2015, &Source, &Target, &DatumCode)!=0)
  {
  printf("ITM Easting: %f\n", Target.X);
  printf("ITM Northing: %f\n", Target.Y);
  printf("Elevation: %f\n", Target.Z);
  printf("Datum Code: %i\n", DatumCode);
  }

return 0;
}