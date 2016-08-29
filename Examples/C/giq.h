#ifndef giq_h__
#define giq_h__

typedef struct coordinates_ 
{
double X;
double Y;
double Z;
} coordinates;

// Parameters: SRIDSource, SRIDTarget, RevisionSource, RevisionTarget, Source Coordinates, Target Coordinates, Datum Code
extern int ConvertCoordinates(int, int, int, int, coordinates *, coordinates *, int *); 

#endif