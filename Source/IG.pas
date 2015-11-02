Unit IG;

{ Irish Grid Unit.

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
  Math, Geometry, Geodesy, OSTab;

{ Define Irish Grid accuracy level. }
//{$DEFINE LEVEL1}  { 2m horizontal accuracy using mean offset adjustments. }
{$DEFINE LEVEL2}  { 0.4m horizontal accuracy using the OSi/OSNI polynomial transformation. }

{ Define to embed the data table within the executable. }
//{$DEFINE EMBED} // TODO If needed?

{ Define to uses inline optimisation. }
//{$DEFINE USE_INLINE}

Type
  TIGCoordinateSystem = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  Airy1830ModifiedEllipsoid: TEllipsoid;
  GRS80Ellipsoid: TEllipsoid;
  IrishGridProjection: TProjection;
  IrishGPSGridProjection: TProjection;
  IGCoordinateSystem: TIGCoordinateSystem;

Const
  IGBounds: TGeodeticBounds = (Western: -10.56*PI/180; Southern: 51.39*PI/180; Eastern: -5.34*PI/180; Northern: 55.43*PI/180);

{$IFDEF LEVEL1}
{ Mean offset of planar Irish Grid to Irish GPS Grid coordinates and average ellipsoid geoid separation. }
Const
  MeanGridOffset: TPlanarCoordinates = (X: 49.0; Y: -23.4);
  MeanGeoidSeparation: TCoordinate = 57.24;
{$ENDIF}

{$IFDEF LEVEL2}
{ OSNI/OSi 3rd order polynomial transformation coefficients. }
Const
  A00 = 0.763;  A01 = 0.123;   A02 = 0.183;   A03 = -0.374;
  A10 = -4.487; A11 = -0.515;  A12 = 0.414;   A13 = 13.110;
  A20 = 0.215;  A21 = -0.57;   A22 = 5.703;   A23 = 113.743;
  A30 = -0.265; A31 = 2.852;   A32 = -61.678; A33 = -265.898;
  B00 = -2.81;  B01 = -4.68;   B02 = 0.170;   B03 = 2.163;
  B10 =-0.341;  B11 =-0.119;   B12 = 3.913;   B13 = 18.867;
  B20 = 1.196;  B21 = 4.877;   B22 = -27.795; B23 = -284.294;
  B30 = -0.887; B31 = -46.666; B32 = -95.377; B33 = -853.95;
{$ENDIF}

{$IFDEF LEVEL2}
Function IGToETRSGeodeticShift(Const Coordinates: TCoordinates): TCoordinates; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Function ETRSToIGGeodeticShift(Const Coordinates: TCoordinates): TCoordinates; {$IFDEF USE_INLINE}Inline;{$ENDIF}
{$ENDIF}
Function WGS84CoordinatesToIGCoordinates(Const Coordinates: TCoordinates; Const GMData: TVerticalTable): TCoordinates;
Function IGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; Const GMData: TVerticalTable): TCoordinates;

Implementation

{$IFDEF LEVEL2}
Uses
  ITM;
{$ENDIF}

Function TIGCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := IGCoordinatesToWGS84Coordinates(Coordinates, GM02NIData);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TIGCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToIGCoordinates(GeodeticCoordinates, GM02NIData);
End;

{$IFDEF LEVEL2}
Function IGToETRSGeodeticShift(Const Coordinates: TCoordinates): TCoordinates; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
 U, U2, U3, V, V2, V3: TCoordinate;
 UV, U2V, UV2, U2V2, U3V: TCoordinate;
 UV3, U3V2, U2V3, U3V3: TCoordinate;
Const
  ScaleFactor = 0.1;
  MeanLatitude = 53.5;
  MeanLongitude = -7.7;
Begin
   U :=ScaleFactor*(RadToDeg(Coordinates.Latitude)-MeanLatitude);
   U2 := U*U;
   U3 := U2*U;
   V :=ScaleFactor*(RadToDeg(Coordinates.Longitude)-MeanLongitude);
   V2 := V*V;
   V3 := V2*V;
   UV := U*V;
   U2V := U2*V;
   UV2 := U*V2;
   U2V2 := U2*V2;
   U3V := U3*V;
   UV3 := U*V3;
   U3V2 := U3*V2;
   U2V3 := U2*V3;
   U3V3 := U3*V3;
   Result.Latitude := DegToRad((A00+A10*U+A01*V+A11*UV+A20*U2+A02*V2+A21*U2V+
                      A12*UV2+A22*U2V2+A30*U3+A03*V3+A31*U3V+A13*UV3+
                      A32*U3V2+A23*U2V3+A33*U3V3)/3600);
   Result.Longitude := DegToRad((B00+B10*U+B01*V+B11*UV+B20*U2+B02*V2+B21*U2V+
                       B12*UV2+B22*U2V2+B30*U3+B03*V3+B31*U3V+B13*UV3+
                       B32*U3V2+B23*U2V3+B33*U3V3)/3600);
   Result.Altitude := Coordinates.Altitude;
End;

Function ETRSToIGGeodeticShift(Const Coordinates: TCoordinates): TCoordinates; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
  DeltaCoordinates: TCoordinates;
  DeltaError: TCoordinates;
  Iteration: Integer;
Const
  IterationLimit = 15;
  Epsilon = 0.0000000000000001;
Begin
  Result := Coordinates;
  For Iteration := 1 To IterationLimit Do
    Begin
      DeltaCoordinates := IGToETRSGeodeticShift(Result);
      DeltaError := Result+DeltaCoordinates-Coordinates;
      If (Abs(DeltaError.Latitude)<Epsilon) And (Abs(DeltaError.Longitude)<Epsilon) Then
      	Break;
      Result := Result-DeltaError;
    End;
End;
{$ENDIF}

Function WGS84CoordinatesToIGCoordinates(Const Coordinates: TCoordinates; Const GMData: TVerticalTable): TCoordinates;
Var
  {$IFDEF LEVEL1}
  GridCoordinates: TCoordinates;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates: TCoordinates;
  Parameters: TInterpolationParameters;
Const
  GridScale = 1000;
  {$ENDIF}
Begin
  {$IFDEF LEVEL1}
  GridCoordinates := TransverseMercator(Coordinates, IrishGPSGridProjection);
  Result := GridCoordinates+MeanGridOffset;
  Result.Altitude := Result.Altitude-MeanGeoidSeparation; { Geoid is below ellipsoid. }
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates := ETRSToIGGeodeticShift(Coordinates);
  Result := TransverseMercator(GeodeticCoordinates, IrishGridProjection);
  //Parameters := BilinearGridInterpolationParameters(GMData.Header.Origin, Result, GridScale);
  //Result.Altitude := Result.Altitude-InterpolateVerticalTable(GMData, Parameters); { Geoid is below ellipsoid. }
  {$ENDIF}
End;

Function IGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; Const GMData: TVerticalTable): TCoordinates;
Var
  {$IFDEF LEVEL1}
  GridCoordinates: TCoordinates;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates: TCoordinates;
  {$ENDIF}
  Parameters: TInterpolationParameters;
Const
  GridScale = 1000;
Begin
  Parameters := BilinearGridInterpolationParameters(GMData.Header.Origin, Coordinates, GridScale);
  {$IFDEF LEVEL1}
  GridCoordinates := Coordinates-MeanGridOffset;
  Result := InverseTransverseMercator(GridCoordinates, IrishGPSGridProjection);
  Result.Altitude := Result.Altitude+MeanGeoidSeparation; { Geoid is below ellipsoid. }
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates := InverseTransverseMercator(Coordinates, IrishGridProjection);
  Result := GeodeticCoordinates+IGToETRSGeodeticShift(GeodeticCoordinates);
  Result.Altitude := Result.Altitude+InterpolateVerticalTable(GMData, Parameters); { Geoid is below ellipsoid. }
  {$ENDIF}
End;

Initialization

Airy1830ModifiedEllipsoid.Initialize(6377340.1890, 6356034.4470);
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
IrishGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, Airy1830ModifiedEllipsoid);
IrishGPSGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, GRS80Ellipsoid);
IGCoordinateSystem.Initialize('Irish Grid', 'IG75', 'Irish Grid (IG)', 29903, ctProjected, aoXYZ, IGBounds);
CoordinateSystems.Register(IGCoordinateSystem);

End.

