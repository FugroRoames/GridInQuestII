// Minimal Grid Inquest II sharable object example for gcc.
#include <stdio.h>
#include "giq.h"
 
int main(void)
{
coordinates Source;
coordinates Target;
int DatumCode;

printf("Grid InQuest II test output.\n");

// Setup the calling parameter values.
Source.X = -0.12217304764; // Longitude in radians (-7 degrees).
Source.Y = 0.925024503557; // Latitude in radians (53 degrees).
Source.Z = 100; // Altitude in metres.
DatumCode = vdMalinHead; // Malin Head vertical datum.

// Call coordinate converter: ETRS89 Geodetic, Irish Transverse Mercator, No Revision, ITM/GM15, Source, Target, Malin Head datum. 
if (ConvertCoordinates(SRID_ETRS89GD, SRID_ITM, 0, 2015, &Source, &Target, &DatumCode)!=0)
  {
  printf("ITM Easting: %f\n", Target.X);
  printf("ITM Northing: %f\n", Target.Y);
  printf("Elevation: %f\n", Target.Z);
  printf("Datum Code: %i\n", DatumCode);
  }

return 0;
}