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
  SysUtils, Math, Geometry, Geodesy, OSTab;

{ Define British National Grid accuracy level options. }
{$DEFINE LEVEL1}  { 10m horizontal accuracy using mean offset adjustments. }
//{$DEFINE LEVEL2} { 3m horizontal accuracy using 100km grid interpolation. }
//{$DEFINE LEVEL3} { 0.5m horizontal accuracy using 10km grid interpolation. }
//{$DEFINE LEVEL4} { 0.1m horizontal accuracy using full 1km grid interpolation. }

{ Define to embed the data table within the executable. }
//{$DEFINE EMBED}

Type
  TBNGCoordinateSystem02 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type
  TBNGCoordinateSystem10 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GM02GBData: TVerticalTable;
  TN02GBData: THorizontalTable;
  GRS80Ellipsoid: TEllipsoid;
  OSTN02GridProjection: TProjection;
  BNGCoordinateSystem02: TBNGCoordinateSystem02;
  BNGCoordinateSystem10: TBNGCoordinateSystem10;

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;

Implementation

{ Embed the required data tables using resource files. }
{$IFDEF EMBED}
  {$IFDEF LEVEL2}
    {$IFDEF Darwin}
      {$R TN02GB100.res}
      {$R VRF10GB100.res}
      {$R GM02GB100.res}
    {$ELSE}
      {$R TN02GB100.rc}
      {$R VRF10GB100.rc}
      {$R GM02GB100.rc}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LEVEL3}
    {$IFDEF Darwin}
      {$R TN02GB10.res}
      {$R VRF10GB10.res}
      {$R GM02GB10.res}
    {$ELSE}
      {$R TN02GB10.rc}
      {$R VRF10GB10.rc}
      {$R GM02GB10.rc}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LEVEL4}
    {$IFDEF Darwin}
      {$R TN02GB.res}
      {$R VRF10GB.res}
      {$R GM02GB.res}
    {$ELSE}
      {$R TN02GB.rc}
      {$R VRF10GB.rc}
      {$R GM02GB.rc}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

Var
  ProgramFolder: String;
  TN02FileName: String;
  VRF10FileName: String;
  GM02FileName: String;
  TN02DataFound: Boolean;
  VRF10DataFound: Boolean;
  GM02DataFound: Boolean;

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Var
  EastOffset, NorthOffset, GeoidHeight: TCoordinate;
Begin
  { Full transformation of GRS80/WGS84/ETRS89 value to OSGB36 easting and northing as per OS method. }
  { 1. Perform transverse Mercator projection of initial ellipsoid using OS grid but WGS84 ellipsiod parameters, Lat/Lon MUST be in radians. }
  Result := TransverseMercator(Coordinates, OSTN02GridProjection);
{  If TN02DataFound Then
  Else If VRF10DataFound And (DatumModel=OSVRF10) Then
  Else If GM02DataFound And (DatumModel=OSGM02) Then
  Else  }
  { 2. Perform bi-linear interpolation of coordinate deltas from OSTN02 and OSGM02 reference table. }
{$IFDEF LEVEL4}
  { Perform full kilometer resolution polynomial correction. }
  { This is the intrinsic conversion as defined by TN02 but requires a 20Mb data table. }
  BilinearInterpolate(TN02GBData, Result, 1000, EastOffset, NorthOffset, GeoidHeight);
{$ENDIF}
{$IFDEF LEVEL3}
  { Perform 10 kilometer resolution polynomial correction. }
  { Max Easting Error: 0.371 Max Northing Error: 0.375 Max Geoid Height Error: 0.554 }
  BilinearInterpolate(TN02GBData, Result, 10000, EastOffset, NorthOffset, GeoidHeight);
{$ENDIF}
{$IFDEF LEVEL2}
  { Perform 100 kilometer resolution polynomial correction. }
  { Max Easting Error: 2.790 Max Northing Error: 2.784 Max Geoid Height Error: 2.389 }
  BilinearInterpolate(TN02GBData, Result, 100000, EastOffset, NorthOffset, GeoidHeight);
{$ENDIF}
{$IFDEF LEVEL1}
  { Instead of a polynomial correction, apply mean deltas which yeild a standard deviation of less than 10m in all directions. }
  {           Easting Shift  Northing Shift  Geoid Height  }
  { Min.          86.27         -81.60          43.98      }
  { Mean          96.04         -68.87          51.16      }
  { Max.         103.44         -50.16          57.66      }
  { St. Dev.       3.29           9.45           3.47      }
  EastOffset := 96.04;
  NorthOffset := -68.87;
  GeoidHeight := 51.16;
{$ENDIF}
With Result Do
  Begin
    Easting := Easting+EastOffset;
    Northing := Northing+NorthOffset;
    Elevation := Elevation-GeoidHeight; { Geoid height is subtracted. }
  End;
End;

Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
{$IFNDEF LEVEL1}
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
  {$IFDEF LEVEL1}
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
      Elevation := Elevation+51.16; { Geoid height is added. }
    End;
  Result := InverseTransverseMercator(Result, OSTN02GridProjection);
  {$ELSE}
  PriorResult := NullCoordinates;
  For Iteration := 1 To IterationLimit Do
    Begin
      {$IFDEF LEVEL2}
        { Perform full kilometer resolution polynomial correction. }
        { This is the intrinsic conversion as defined by TN02 but requires a 20Mb data table. }
        BilinearInterpolate(TN02GBData, Result, 1000, EastOffset, NorthOffset, GeoidHeight);
      {$ENDIF}
      {$IFDEF LEVEL3}
        { Perform 10 kilometer resolution polynomial correction. }
        { Max Easting Error: 0.371 Max Northing Error: 0.375 Max Geoid Height Error: 0.554 }
        BilinearInterpolate(TN02GBData, Result, 10000, EastOffset, NorthOffset, GeoidHeight);
      {$ENDIF}
      {$IFDEF LEVEL4}
        { Perform 100 kilometer resolution polynomial correction. }
        { Max Easting Error: 2.790 Max Northing Error: 2.784 Max Geoid Height Error: 2.389 }
        BilinearInterpolate(TN02GBData, Result, 100000, EastOffset, NorthOffset, GeoidHeight);
      {$ENDIF}
      With Result Do
        Begin
          Easting := Coordinates.Easting-EastOffset;
          Northing := Coordinates.Northing-NorthOffset;
          Elevation := Coordinates.Elevation+GeoidHeight;
        End;
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

{$IFDEF EMBED}
Procedure LoadResourceTables;
Var
  ResourceStream: TStream;
Begin
  { If no external data files found, load the tables from the embedded resources. }
  If Not TN02DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'TN02GB', 'DATA');
      TN02DataFound := TN02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
  If Not VRF10DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'VRF10GB', 'DATA');
      VRF10DataFound := GM02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
  If Not GM02DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'GM02GB', 'DATA');
      GM02DataFound := GM02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
End;
{$ENDIF}

Initialization

ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$IFDEF LEVEL1}
TN02DataFound := False;
VRF10DataFound := False;
GM02DataFound := False;
{$ENDIF}
{$IFDEF LEVEL2}
TN02FileName := ProgramFolder+'TN02GB100.dat';
VRF10FileName := ProgramFolder+'VRF10GB100.dat';
GM02FileName := ProgramFolder+'GM02GB100.dat';
{$ENDIF}
{$IFDEF LEVEL3}
TN02FileName := ProgramFolder+'TN02GB10.dat';
VRF10FileName := ProgramFolder+'VRF10GB10.dat';
GM02FileName := ProgramFolder+'GM02GB10.dat';
{$ENDIF}
{$IFDEF LEVEL4}
TN02FileName := ProgramFolder+'TN02GBTest.dat';
VRF10FileName := ProgramFolder+'VRF10GBTest.dat';
GM02FileName := ProgramFolder+'GM02GBTest.dat';
{$ENDIF}
{$IFNDEF LEVEL1}
TN02DataFound := TN02GBData.LoadFromFile(TN02FileName);
VRF10DataFound := GM02GBData.LoadFromFile(VRF10FileName);
GM02DataFound := GM02GBData.LoadFromFile(GM02FileName);
{$ENDIF}
{$IFDEF EMBED}
LoadResourceTables;
{$ENDIF}
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
OSTN02GridProjection.Initialize(0.9996012717, DegToRad(49), DegToRad(-2), 400000, -100000, GRS80Ellipsoid);
BNGCoordinateSystem02.Initialize('British National Grid (2002)', 'OSGB36',
                                 'OSGB36 / British National Grid (TN02/GM02)', 27700, ctCartesian, aoXYZ);
BNGCoordinateSystem10.Initialize('British National Grid (2010)', 'OSGB36',
                                 'OSGB36 / British National Grid (TN02/VRF10)', 27700, ctCartesian, aoXYZ);
CoordinateSystems.Register(BNGCoordinateSystem02);
CoordinateSystems.Register(BNGCoordinateSystem10);

End.
