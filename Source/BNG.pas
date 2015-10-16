Unit BNG;

{ British National Grid Unit.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  SysUtils, Math, Geometry, Geodesy, OSMod;

Type TBNGCoordinateSystem02 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type TBNGCoordinateSystem10 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GRS80Ellipsoid: TEllipsoid;
  OSTN02GridProjection: TProjection;
  BNGCoordinateSystem02: TBNGCoordinateSystem02;
  BNGCoordinateSystem10: TBNGCoordinateSystem10;

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;

Implementation

{$IFDEF OSTNO2TABLEUSED}
Procedure BilinearInterpolate(Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);
Var
  InvGridScale: TCoordinate;
  X1, X2, Y1, Y2: Integer;
  SE0, SE1, SE2, SE3: TCoordinate;
  SN0, SN1, SN2, SN3: TCoordinate;
  SG0, SG1, SG2, SG3: TCoordinate;
  T, TI, U, UI: TCoordinate;
Begin
  InvGridScale := 1/GridScale;
  X1 := Trunc(Coordinates.Easting*InvGridScale);
  Y1 := Trunc(Coordinates.Northing*InvGridScale);
  X2 := X1+1;
  Y2 := Y1+1;
  With OSTN02Data[X1, Y1] Do
    Begin
      SE0 := Easting;
      SN0 := Northing;
      SG0 := Elevation;
    End;
  With OSTN02Data[X2, Y1] Do
    Begin
      SE1 := Easting;
      SN1 := Northing;
      SG1 := Elevation;
    End;
  With OSTN02Data[X2, Y2] Do
    Begin
      SE2 := Easting;
      SN2 := Northing;
      SG2 := Elevation;
    End;
  With OSTN02Data[X1, Y2] Do
    Begin
      SE3 := Easting;
      SN3 := Northing;
      SG3 := Elevation;
    End;
  T := (Coordinates.Easting-(X1*GridScale))*InvGridScale;
  TI := (1-T);
  U := (Coordinates.Northing-(Y1*GridScale))*InvGridScale;
  UI := (1-U);
  EastOffset := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
  NorthOffset := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
  GeoidHeight := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
End;
{$ENDIF}

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Var
  EastOffset, NorthOffset, GeoidHeight: TCoordinate;
Begin
  { Full transformation of GRS80/WGS84/ETRS89 value to OSGB36 easting and northing as per OS method. }
  { 1. Perform transverse Mercator projection of initial ellipsoid using OS grid but WGS84 ellipsiod parameters, Lat/Lon MUST be in radians. }
  Result := TransverseMercator(Coordinates, OSTN02GridProjection);
  { 2. Perform bi-linear interpolation of coordinate deltas from OSTN02 and OSGM02 reference table. }
{$IFDEF OSTNO2RES_1KM}
  { Perform full kilometer resolution polynomial correction. }
  { This is the intrinsic conversion as defined by TN02 but requires a 20Mb data table. }
  BilinearInterpolate(Result, 1000, EastOffset, NorthOffset, GeoidHeight);
  With Result Do
    Begin
      Easting := Easting+EastOffset;
      Northing := Northing+NorthOffset;
      Elevation := Elevation-GeoidHeight; { Geoid height is subtracted. }
    End;
{$ENDIF}
{$IFDEF OSTNO2RES_10KM}
  { Perform 10 kilometer resolution polynomial correction. }
  { Max Easting Error: 0.371 Max Northing Error: 0.375 Max Geoid Height Error: 0.554 }
  BilinearInterpolate(Result, 10000, EastOffset, NorthOffset, GeoidHeight);
  With Result Do
    Begin
      Easting := Easting+EastOffset;
      Northing := Northing+NorthOffset;
      Elevation := Elevation-GeoidHeight;  { Geoid height is subtracted. }
    End;
{$ENDIF}
{$IFDEF OSTNO2RES_100KM}
  { Perform 100 kilometer resolution polynomial correction. }
  { Max Easting Error: 2.790 Max Northing Error: 2.784 Max Geoid Height Error: 2.389 }
  BilinearInterpolate(Result, 100000, EastOffset, NorthOffset, GeoidHeight);
  With Result Do
    Begin
      Easting := Easting+EastOffset;
      Northing := Northing+NorthOffset;
      Elevation := Elevation-GeoidHeight; { Geoid height is subtracted. }
    End;
{$ENDIF}
{$IFNDEF OSTNO2TABLEUSED}
  { Instead of a polynomial correction, apply mean deltas which yeild a standard deviation of less than 10m in all directions. }
  {           Easting Shift  Northing Shift  Geoid Height  }
  { Min.          86.27         -81.60          43.98      }
  { Mean          96.04         -68.87          51.16      }
  { Max.         103.44         -50.16          57.66      }
  { St. Dev.       3.29           9.45           3.47      }
  With Result Do
    Begin
      Easting := Easting+96.04;
      Northing := Northing-68.87;
      Elevation := Elevation-51.16; { Geoid height is subtracted. }
    End;
{$ENDIF}
End;

Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
{$IFDEF OSTNO2TABLEUSED}
Var
  EastOffset, NorthOffset, GeoidHeight: TCoordinate;
  PriorResult: TCoordinates;
  Iteration: Integer;
Const
  IterationLimit = 6;
  Epsilon: TCoordinate = 0.0001;
{$ENDIF}
Begin
  Result := Coordinates;
  {$IFDEF OSTNO2TABLEUSED}
  PriorResult := NullCoordinates;
  For Iteration := 1 To IterationLimit Do
    Begin
      {$IFDEF OSTNO2RES_1KM}
        { Perform full kilometer resolution polynomial correction. }
        { This is the intrinsic conversion as defined by TN02 but requires a 20Mb data table. }
        BilinearInterpolate(Result, 1000, EastOffset, NorthOffset, GeoidHeight);
        With Result Do
          Begin
            Easting := Coordinates.Easting-EastOffset;
            Northing := Coordinates.Northing-NorthOffset;
            Elevation := Coordinates.Elevation+GeoidHeight;
          End;
      {$ENDIF}
      {$IFDEF OSTNO2RES_10KM}
        { Perform 10 kilometer resolution polynomial correction. }
        { Max Easting Error: 0.371 Max Northing Error: 0.375 Max Geoid Height Error: 0.554 }
        BilinearInterpolate(Result, 10000, EastOffset, NorthOffset, GeoidHeight);
        With Result Do
          Begin
            Easting := Coordinates.Easting-EastOffset;
            Northing := Coordinates.Northing-NorthOffset;
            Elevation := Coordinates.Elevation+GeoidHeight;
          End;
      {$ENDIF}
      {$IFDEF OSTNO2RES_100KM}
        { Perform 100 kilometer resolution polynomial correction. }
        { Max Easting Error: 2.790 Max Northing Error: 2.784 Max Geoid Height Error: 2.389 }
        BilinearInterpolate(Result, 100000, EastOffset, NorthOffset, GeoidHeight);
        With Result Do
          Begin
            Easting := Coordinates.Easting-EastOffset;
            Northing := Coordinates.Northing-NorthOffset;
            Elevation := Coordinates.Elevation+GeoidHeight;
          End;
      {$ENDIF}
      { If convergance has been reached. }
      If (Abs(Result.Easting-PriorResult.Easting)<Epsilon) And
         (Abs(Result.Northing-PriorResult.Northing)<Epsilon) Then
         Begin
           Result := InverseTransverseMercator(Result, OSTN02GridProjection);
           Exit;
         End;
      PriorResult := Result;
    End;
  Raise Exception.Create('Inverse BNG projection failed to converge grid to ETRS89 coordinates.');
  {$ELSE}
  { Instead of a polynomial correction, apply mean deltas which yeild a standard deviation of less than 10m in all directions. }
  {           Easting Shift  Northing Shift  Geoid Height  }
  { Min.          86.27         -81.60          43.98      }
  { Mean          96.04         -68.87          51.16      }
  { Max.         103.44         -50.16          57.66      }
  { St. Dev.       3.29           9.45           3.47      }
  With Result Do
    Begin
      Easting := Easting-96.04;
      Northing := Northing+68.87;
      Elevation := Elevation+51.16; { Geoid height is subtracted. }
    End;
  {$ENDIF}
End;

Function TBNGCoordinateSystem02.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := BNGCoordinatesToWGS84Coordinates(Coordinates);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TBNGCoordinateSystem02.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToBNGCoordinates(GeodeticCoordinates);
End;

Function TBNGCoordinateSystem10.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := BNGCoordinatesToWGS84Coordinates(GeodeticCoordinates);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TBNGCoordinateSystem10.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToBNGCoordinates(GeodeticCoordinates);
End;

Initialization

//Airy1830Ellipsoid.Initialize(6377563.3960, 6356256.9100);
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
OSTN02GridProjection.Initialize(0.9996012717, DegToRad(49), DegToRad(-2), 400000, -100000, GRS80Ellipsoid);
BNGCoordinateSystem02.Initialize('British National Grid (2002)', 'OSGB36',
                                 'OSGB36 / British National Grid (BNG/GM02)', 27700, ctCartesian, aoXYZ);
BNGCoordinateSystem10.Initialize('British National Grid (2010)', 'OSGB36',
                                 'OSGB36 / British National Grid (BNG/VRF10)', 27700, ctCartesian, aoXYZ);
CoordinateSystems.Register(BNGCoordinateSystem02);
CoordinateSystems.Register(BNGCoordinateSystem10);

End.

