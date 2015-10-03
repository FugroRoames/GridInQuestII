Unit ETRS;

{ European Terrestrial Reference System Unit.

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
  Math, Geometry, Geodesy;

Type TETRS89CoordinateSystemGC = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ETRS89CoordinateSystemGC: TETRS89CoordinateSystemGC =
    (
      Abbreviation: 'ETRS89';
      AxisOrder: aoXYZ;
      CoordinateType: ctGeocentric;
      Description: 'ETRS89 Geocentric';
      EPSGNumber: 4936;
      Name: 'ETRS89 Geocentric';
    );

Type TETRS89CoordinateSystemGD = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ETRS89CoordinateSystemGD: TETRS89CoordinateSystemGD =
    (
      Abbreviation: 'ETRS89';
      AxisOrder: aoYXZ;
      CoordinateType: ctGeodetic;
      Description: 'ETRS89 Geodetic';
      EPSGNumber: 4937;
      Name: 'ETRS89 Geodetic';
    );

Type TETRS89CoordinateSystem29N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ETRS89CoordinateSystem29N: TETRS89CoordinateSystem29N =
    (
      Abbreviation: 'UTM29N';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'ETRS89 / UTM Zone 29N';
      EPSGNumber: 25829;
      Name: 'ETRS89 UTM 29N';
    );

Type TETRS89CoordinateSystem30N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ETRS89CoordinateSystem30N: TETRS89CoordinateSystem30N =
    (
      Abbreviation: 'UTM30N';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'ETRS89 / UTM Zone 30N';
      EPSGNumber: 25830;
      Name: 'ETRS89 UTM 30N';
    );

Type TETRS89CoordinateSystem31N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ETRS89CoordinateSystem31N: TETRS89CoordinateSystem31N =
    (
      Abbreviation: 'UTM31N';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'ETRS89 / UTM Zone 31N';
      EPSGNumber: 25831;
      Name: 'ETRS89 UTM 31N';
    );

Implementation

Function TETRS89CoordinateSystemGC.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := Coordinates; { No conversion required. }
End;

Function TETRS89CoordinateSystemGC.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := Coordinates; { No conversion required. }
End;

Function TETRS89CoordinateSystemGD.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystemGD.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem29N.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem29N.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem30N.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem30N.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem31N.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TETRS89CoordinateSystem31N.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Initialization

CoordinateSystems.Register(ETRS89CoordinateSystemGC);
CoordinateSystems.Register(ETRS89CoordinateSystemGD);
CoordinateSystems.Register(ETRS89CoordinateSystem29N);
CoordinateSystems.Register(ETRS89CoordinateSystem30N);
CoordinateSystems.Register(ETRS89CoordinateSystem31N);

End.

