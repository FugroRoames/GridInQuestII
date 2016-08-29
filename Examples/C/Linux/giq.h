// Grid InQuest II sharable object interface.
#ifndef giq_h__
#define giq_h__

// SRID definitions for supported coordinate systems.
#define SRID_ETRS89CT  4936
#define SRID_ETRS89GD  4937
#define SRID_UTM29N   25829
#define SRID_UTM30N   25830
#define SRID_UTM31N   25831
#define SRID_BNG      27700
#define SRID_IG       29903
#define SRID_ITM       2157

// Vertical datum codes.
enum GIQDatums
{
vdNone = 0, 
vdOrdnanceDatumNewlyn, 
vdStMarys, 
vdDouglas02,
vdStornoway, 
vdStKilda, 
vdLerwick, 
vdOrkney, 
vdFairIsle,
vdFlannanIsles, 
vdNorthRona, 
vdSuleSkerry, 
vdFoula,
vdMalinHead, 
vdBelfast, 
vdOffshore, 
vdOutside
};

// Coordinates structure.
typedef struct coordinates_ 
{
double X;
double Y;
double Z;
} coordinates;

// Parameters: SRIDSource, SRIDTarget, RevisionSource, RevisionTarget, Source Coordinates, Target Coordinates, Datum Code.
extern int ConvertCoordinates(int, int, int, int, coordinates *, coordinates *, int *); 

#endif