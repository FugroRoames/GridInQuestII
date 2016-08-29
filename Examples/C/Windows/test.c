// Minimal Grid Inquest II DLL example for Visual C++
// Original Visual C++ conversion by Shane MacLaughlin, Atlas Computers Ltd

#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include "giq.h"

int main(void)
{
printf("Simple test for GridInquest II DLL\n");

// Load DLL, change to specific directory for x86 or x64 as necessary.
HMODULE GIQHandle = LoadLibrary(_T("GIQ.DLL"));
if (!GIQHandle)
  {
  printf("Failed to load GIQ.DLL\n");
  return -1;
  }
  
// Get entry point for conversion function.
PConvertCoordinates pConvertCoords = (PConvertCoordinates)GetProcAddress(GIQHandle, "ConvertCoordinates");
if (!pConvertCoords)
  {
  printf("Failed to find ConvertCoordinates entry point\n");
  return -2;
  }

coordinates Source;
coordinates Target;
int DatumCode;

// Setup the calling parameter values.
Source.X = -0.12217304764; // Longitude in radians (-7 degrees).
Source.Y = 0.925024503557; // Latitude in radians (53 degrees).
Source.Z = 100; // Altitude in metres.
DatumCode = vdMalinHead; // Malin Head vertical datum.

// Call coordinate converter: ETRS89 Geodetic, Irish Transverse Mercator, No Revision, ITM/GM15, Source, Target, Malin Head datum.
if (pConvertCoords(SRID_ETRS89GD, SRID_ITM, 0, 2015, &Source, &Target, &DatumCode) != 0)
  {
  printf("ITM Easting: %f\n", Target.X);
  printf("ITM Northing: %f\n", Target.Y);
  printf("Elevation: %f\n", Target.Z);
  printf("Datum Code: %i\n", DatumCode);
  }

// Release the dynamic library.
FreeLibrary(GIQHandle);
return 0;
}