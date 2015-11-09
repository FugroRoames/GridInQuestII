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
    PreferredVerticalDatum: TVerticalDatumCode;
    LastVerticalDatum: TVerticalDatumCode;
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                           NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                           NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode);
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
Function WGS84CoordinatesToIGCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Function IGCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;

Implementation

{$IFDEF LEVEL2}
Uses
  ITM;
{$ENDIF}

Constructor TIGCoordinateSystem.Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                                           NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                                           NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode);
Begin
  Name := NewName;
  Abbreviation := NewAbbreviation;
  Description := NewDescription;
  EPSGNumber := NewEPSGNumber;
  CoordinateType := NewCoordinateType;
  AxisOrder := NewAxisOrder;
  GeodeticBounds := NewBounds;
  PreferredVerticalDatum := NewPreferredVerticalDatum;
End;

Function TIGCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  // Test for bounds?
  If IGCoordinatesToWGS84Coordinates(Coordinates, vmGM02, PreferredVerticalDatum, GeodeticCoordinates, LastVerticalDatum) Then
    Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid)
  Else
    Result := NullCoordinates;
End;

Function TIGCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  // Test for bounds?
  If Not WGS84CoordinatesToIGCoordinates(GeodeticCoordinates, vmGM02, PreferredVerticalDatum, Result, LastVerticalDatum) Then
    Result := NullCoordinates;
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
   Result.Altitude := 0; { No change to geoid height in this model. }
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

Function WGS84CoordinatesToIGCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Var
  {$IFDEF LEVEL1}
  GridCoordinates: TCoordinates;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates: TCoordinates;
  ITMCoordinates: TCoordinates;
  {$ENDIF}
Begin
  {$IFDEF LEVEL1}
  GridCoordinates := TransverseMercator(InputCoordinates, IrishGPSGridProjection);
  OutputCoordinates := GridCoordinates+MeanGridOffset;
  OutputCoordinates.Elevation := OutputCoordinates.Elevation-MeanGeoidSeparation; { Geoid is below ellipsoid. }
  OutputDatum := vdNone; { Level 1 cannot associate heights with a specific datum, but applies a mean separation value. }
  Result := True;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates := ETRSToIGGeodeticShift(InputCoordinates);
  OutputCoordinates := TransverseMercator(GeodeticCoordinates, IrishGridProjection);
  { Determine location validity from the corresponding ITM coordinates, and use that geoid height if valid. }
  Result := WGS84CoordinatesToITMCoordinates(InputCoordinates, vmGM02, PreferredDatum, ITMCoordinates, OutputDatum);
  If Result Then
    OutputCoordinates.Elevation := ITMCoordinates.Elevation;
  {$ENDIF}
End;

Function IGCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
 Var
  {$IFDEF LEVEL1}
  GridCoordinates: TCoordinates;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates: TCoordinates;
  ITMCoordinates: TCoordinates;
  {$ENDIF}
Begin
  {$IFDEF LEVEL1}
  GridCoordinates := InputCoordinates-MeanGridOffset;
  OutputCoordinates := InverseTransverseMercator(GridCoordinates, IrishGPSGridProjection);
  OutputCoordinates.Altitude := OutputCoordinates.Altitude+MeanGeoidSeparation; { Geoid is below ellipsoid. }
  OutputDatum := vdNone; { Level 1 cannot associate heights with a specific datum, but applies a mean separation value. }
  Result := True;
  {$ENDIF}
  {$IFDEF LEVEL2}
  GeodeticCoordinates := InverseTransverseMercator(InputCoordinates, IrishGridProjection);
  GeodeticCoordinates.Altitude := 0; { Remove the altitude component. }
  OutputCoordinates := GeodeticCoordinates+IGToETRSGeodeticShift(GeodeticCoordinates);
  { Determine location validity from the corresponding ITM coordinates, and the corresponding geoid height if valid. }
  Result := WGS84CoordinatesToITMCoordinates(OutputCoordinates, vmGM02, PreferredDatum, ITMCoordinates, OutputDatum);
  If Result Then
    OutputCoordinates.Altitude := InputCoordinates.Elevation-ITMCoordinates.Altitude; { Output the elevation with geoid height correction. }
  {$ENDIF}
End;

Initialization

Airy1830ModifiedEllipsoid.Initialize(6377340.1890, 6356034.4470);
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
IrishGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, Airy1830ModifiedEllipsoid);
IrishGPSGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, GRS80Ellipsoid);
IGCoordinateSystem.Initialize('Irish Grid', 'IG75', 'Irish Grid (IG)', 29903, ctProjected, aoXYZ, IGBounds, vdMalinHead);
CoordinateSystems.Register(IGCoordinateSystem);

End.

