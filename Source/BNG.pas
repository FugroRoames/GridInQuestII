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

{ Level 1 simply applies mean deltas instead of using the polynomial correction. }
{ This method yeilds a standard deviation of less than 10m in all directions. }
{           Easting Shift  Northing Shift  Geoid Height  }
{ Min.          86.27         -81.60          43.98      }
{ Mean          96.04         -68.87          51.16      }
{ Max.         103.44         -50.16          57.66      }
{ St. Dev.       3.29           9.45           3.47      }
//{$DEFINE LEVEL1}
{ Level 2 performs 100 kilometer resolution polynomial correction. }
{ This achieves better than 3m horizontal accuracy. }
{ Max Easting Error: 2.790 Max Northing Error: 2.784 Max Geoid Height Error: 2.389 }
//{$DEFINE LEVEL2}
{ Level 3 performs 10 kilometer resolution polynomial correction. }
{ This achieves better than 0.5m horizontal accuracy. }
{ Max Easting Error: 0.371 Max Northing Error: 0.375 Max Geoid Height Error: 0.554 }
//{$DEFINE LEVEL3}
{ Level 4 performs full kilometer resolution polynomial correction. }
{ This achieves better than 0.1m horizontal accuracy. }
{ This is the published method defined by OS but requires 20Mb of additional data tables. }
{$DEFINE LEVEL4}

{ Define to embed the data table within the executable. }
//{$DEFINE EMBED}

Type
  TBNG02CoordinateSystem = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type
  TBNG15CoordinateSystem = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GM02GBData: TVerticalTable;
  GM15GBData: TVerticalTable;
  TN02GBData: THorizontalTable;
  TN15GBData: THorizontalTable;
  GRS80Ellipsoid: TEllipsoid;
  BNGGridProjection: TProjection;
  BNG02CoordinateSystem: TBNG02CoordinateSystem;
  BNG15CoordinateSystem: TBNG15CoordinateSystem;

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; Const TNData: THorizontalTable; Const GMData: TVerticalTable): TCoordinates;
Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; Const TNData: THorizontalTable; Const GMData: TVerticalTable): TCoordinates;

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
  TN15FileName: String;
  GM02FileName: String;
  GM15FileName: String;
  TN02DataFound: Boolean;
  TN15DataFound: Boolean;
  GM02DataFound: Boolean;
  GM15DataFound: Boolean;

Function WGS84CoordinatesToBNGCoordinates(Const Coordinates: TCoordinates; Const TNData: THorizontalTable; Const GMData: TVerticalTable): TCoordinates;
Var
{$IFNDEF LEVEL1}Parameters: TInterpolationParameters;{$ENDIF}
  Shifts: TCoordinates;
{$IFDEF LEVEL2}Const GridScale = 100000;{$ENDIF}
{$IFDEF LEVEL3}Const GridScale = 10000;{$ENDIF}
{$IFDEF LEVEL4}Const GridScale = 1000;{$ENDIF}
Begin
  { Full transformation of GRS80/WGS84/ETRS89 value to OSGB36 easting and northing as per OS method. }
  { 1. Perform transverse Mercator projection of initial ellipsoid using OS grid but WGS84 ellipsiod parameters, Lat/Lon MUST be in radians. }
  Result := TransverseMercator(Coordinates, BNGGridProjection);
  { 2. Perform coordinate corrections either from mean shift or from interpolation of the OS data tables. }
{$IFDEF LEVEL1}
  Shifts.Easting := 96.04;
  Shifts.Northing := -68.87;
  Shifts.Altitude := -51.16; { Geoid height is subtracted. }
{$ELSE}
  Parameters := BilinearGridInterpolationParameters(TNData.Header.Origin, Result, GridScale);
  Shifts := InterpolateHorizontalTable(TNData, Parameters);
  Shifts.Altitude := -InterpolateVerticalTable(GMData, Parameters); { Geoid is below ellipsoid. }
{$ENDIF}
  Result := Result+Shifts;
End;

Function BNGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; Const TNData: THorizontalTable; Const GMData: TVerticalTable): TCoordinates;
{$IFNDEF LEVEL1}
Var
  Parameters: TInterpolationParameters;
  Shifts: TCoordinates;
  PriorResult: TCoordinates;
  Iteration: Integer;
Const
  IterationLimit = 6;
  Epsilon: TCoordinate = 0.0001;
{$ENDIF}
{$IFDEF LEVEL2}GridScale = 100000;{$ENDIF}
{$IFDEF LEVEL3}GridScale = 10000;{$ENDIF}
{$IFDEF LEVEL4}GridScale = 1000;{$ENDIF}
Begin
  Result := Coordinates;
  {$IFDEF LEVEL1}
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
      Parameters := BilinearGridInterpolationParameters(TNData.Header.Origin, Result, GridScale);
      Shifts := InterpolateHorizontalTable(TNData, Parameters);
      Shifts.Altitude := -InterpolateVerticalTable(GMData, Parameters); { Geoid is below ellipsoid. }
      Result := Coordinates-Shifts;
      { If convergance has been reached. }
      If (Abs(Result.Easting-PriorResult.Easting)<Epsilon) And
         (Abs(Result.Northing-PriorResult.Northing)<Epsilon) Then
         Begin
           Result := InverseTransverseMercator(Result, BNGGridProjection);
           Exit;
         End;
      PriorResult := Result;
    End;
  Raise Exception.Create('Inverse BNG projection failed to converge grid to ETRS89 coordinates.');
  {$ENDIF}
End;

Function TBNG02CoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := BNGCoordinatesToWGS84Coordinates(Coordinates, TN02GBData, GM02GBData);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TBNG02CoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToBNGCoordinates(GeodeticCoordinates, TN02GBData, GM02GBData);
End;

Function TBNG15CoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := BNGCoordinatesToWGS84Coordinates(Coordinates, TN15GBData, GM15GBData);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TBNG15CoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToBNGCoordinates(GeodeticCoordinates, TN15GBData, GM15GBData);
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
  If Not TN15DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'TN15GB', 'DATA');
      VRF10DataFound := GM02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
  If Not GM02DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'GM02GB', 'DATA');
      GM02DataFound := GM02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
  If Not GM15DataFound Then
    Begin
      ResourceStream := TResourceStream.Create(hInstance, 'GM15GB', 'DATA');
      GM02DataFound := GM02GBData.LoadFromStream(ResourceStream);
      FreeAndNil(ResourceStream);
    End;
End;
{$ENDIF}

Initialization

ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$IFDEF LEVEL1}
TN02DataFound := False;
TN15DataFound := False;
GM02DataFound := False;
GM15DataFound := False;
{$ENDIF}
{$IFDEF LEVEL2}
TN02FileName := ProgramFolder+'TN02GB100.dat';
TN15FileName := ProgramFolder+'TN15GB100.dat';
GM02FileName := ProgramFolder+'GM02GB100.dat';
GM15FileName := ProgramFolder+'GM15GB100.dat';
{$ENDIF}
{$IFDEF LEVEL3}
TN02FileName := ProgramFolder+'TN02GB10.dat';
TN15FileName := ProgramFolder+'TN15GB10.dat';
GM02FileName := ProgramFolder+'GM02GB10.dat';
GM15FileName := ProgramFolder+'GM15GB10.dat';
{$ENDIF}
{$IFDEF LEVEL4}
TN02FileName := ProgramFolder+'TN02GB.dat';
TN15FileName := ProgramFolder+'TN15GB.dat';
GM02FileName := ProgramFolder+'GM02GB.dat';
GM15FileName := ProgramFolder+'GM15GB.dat';
{$ENDIF}
{$IFNDEF LEVEL1}
TN02DataFound := TN02GBData.LoadFromFile(TN02FileName);
TN15DataFound := TN15GBData.LoadFromFile(TN15FileName);
GM02DataFound := GM02GBData.LoadFromFile(GM02FileName);
GM15DataFound := GM15GBData.LoadFromFile(GM15FileName);
{$ENDIF}
{$IFDEF EMBED}
LoadResourceTables;
{$ENDIF}
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
BNGGridProjection.Initialize(0.9996012717, DegToRad(49), DegToRad(-2), 400000, -100000, GRS80Ellipsoid);
BNG02CoordinateSystem.Initialize('British National Grid (2002)', 'OSGB36',
                                 'OSGB36 / British National Grid (TN02/GM02)', 27700, ctProjected, aoXYZ);
BNG15CoordinateSystem.Initialize('British National Grid (2015)', 'OSGB36',
                                 'OSGB36 / British National Grid (TN15/GM15)', 27701, ctProjected, aoXYZ);
CoordinateSystems.Register(BNG02CoordinateSystem);
CoordinateSystems.Register(BNG15CoordinateSystem);

End.
