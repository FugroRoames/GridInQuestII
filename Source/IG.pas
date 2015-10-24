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
{$DEFINE LEVEL1}  { 2m horizontal accuracy. }
//{$DEFINE LEVEL2} { 0.4m horizontal accuracy. }

Type TIGCoordinateSystem75 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GRS80Ellipsoid: TEllipsoid;
//  Airy1830ModifiedEllipsoid: TEllipsoid;
//  IrishGridProjection: TProjection;
  IrishGPSGridProjection: TProjection;
  IGCoordinateSystem75: TIGCoordinateSystem75;

{$IFDEF LEVEL1}
{ Mean offset of planar Irish Grid to Irish GPS Grid coordinates. }
Const
  MeanGridOffset: TPlanarCoordinates = (X: 49.0; Y: -23.4);
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
Function IGToETRSShift(Coordinates: TCoordinates): TCoordinates; Inline;
{$ENDIF}
Function WGS84CoordinatesToIGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Function IGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;

Implementation

Function TIGCoordinateSystem75.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := IGCoordinatesToWGS84Coordinates(Coordinates);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TIGCoordinateSystem75.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := WGS84CoordinatesToIGCoordinates(GeodeticCoordinates);
End;

{$IFDEF LEVEL2}
Function IGToETRSShift(Coordinates: TCoordinates): TCoordinates; Inline;
Var
 U, U2, U3, V, V2, V3: TCoordinate;
 UV, U2V, UV2, U2V2, U3V: TCoordinate;
 UV3, U3V2, U2V3, U3V3: TCoordinate;
 Delta: TCoordinates;
Begin
   U :=osik0*(RadToDeg(Coordinates.Latitude)-osilatm);
   U2 := U*U;
   U3 := U2*U;
   V :=osik0*(RadToDeg(Coordinates.Longitude)-osilonm);
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
   Delta.Latitude := (A00+A10*U+A01*V+A11*UV+A20*U2+A02*V2+A21*U2V+
                      A12*UV2+A22*U2V2+A30*U3+A03*V3+A31*U3V+A13*UV3+
                      A32*U3V2+A23*U2V3+A33*U3V3)/3600;
   Delta.Longitude := (B00+B10*U+B01*V+B11*UV+B20*U2+B02*V2+B21*U2V+
                       B12*UV2+B22*U2V2+A30*U3+B03*V3+B31*U3V+B13*UV3+
                       B32*U3V2+B23*U2V3+B33*U3V3)/3600;
  Result := DegToRad(Delta);
End;
{$ENDIF}

Function WGS84CoordinatesToIGCoordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Var
  GridCoordinates: TCoordinates;
  {$IFDEF LEVEL2}
  Shift: TCoordinates;
  {$ENDIF}
Begin
  GridCoordinates := TransverseMercator(Coordinates, IrishGPSGridProjection);
  {$IFDEF LEVEL1}
  Result := GridCoordinates+MeanGridOffset;
  {$ENDIF}
  {$IFDEF LEVEL2}
  Shift := IGToETRSShift(Coordinates);
  //Result := GridCoordinates+Shift;
  {$ENDIF}
  // Add height from GM02/VRF10
End;

Function IGCoordinatesToWGS84Coordinates(Const Coordinates: TCoordinates; DatumModel: TVerticalDatumModel = OSVRF10): TCoordinates;
Var
  GridCoordinates: TCoordinates;
  {$IFDEF LEVEL2}
  Shift: TCoordinates;
  {$ENDIF}
Begin
  {$IFDEF LEVEL1}
  GridCoordinates := Coordinates-MeanGridOffset;
  {$ENDIF}
  {$IFDEF LEVEL2}
  Shift := IGToETRSShift(Coordinates);
  //GridCoordinates := Coordinates-Shift;
  {$ENDIF}
  {$IFDEF LEVEL2}
{
Shift, PriorShift: TCoordinates;
Iteration: Integer;
Const
IterationLimit = 10;
Epsilon: TCoordinate = 1E-6;

ShiftCoordinates := Coordinates;
  For Iteration := 1 To IterationLimit Do
     Begin
       ShiftCoordinates := OSIShift(Coordinates);
       If (Math.abs(shifty.longitude-oldshifty.longitude<Epsilon)
         && Math.abs(shifty.latitude-oldshifty.latitude<Epsilon)) Then
           Exit;
       Result := Coordinates-Shift;
       PriorShift := Shift;
     End;      }
  {$ENDIF}

  Result := InverseTransverseMercator(GridCoordinates, IrishGPSGridProjection);
  // Add height from GM02/VRF10
End;

Initialization

GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
//Airy1830ModifiedEllipsoid.Initialize(6377340.1890, 6356034.4470);
//IrishGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, Airy1830ModifiedEllipsoid);
IrishGPSGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, GRS80Ellipsoid);
IGCoordinateSystem75.Initialize('Irish Grid (1975)', 'TM75IG', 'TM75 / Irish Grid (IG)', 29903, ctCartesian, aoXYZ);
CoordinateSystems.Register(IGCoordinateSystem75);

End.

