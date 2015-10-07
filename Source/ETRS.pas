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

Var
  GRS80Ellipsoid: TEllipsoid;

Type TETRS89CoordinateSystemGC = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  ETRS89CoordinateSystemGC: TETRS89CoordinateSystemGC;

Type TETRS89CoordinateSystemGD = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  ETRS89CoordinateSystemGD: TETRS89CoordinateSystemGD;

Type TETRS89CoordinateSystem29N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  ETRS89CoordinateSystem29N: TETRS89CoordinateSystem29N;

Type TETRS89CoordinateSystem30N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  ETRS89CoordinateSystem30N: TETRS89CoordinateSystem30N;

Type TETRS89CoordinateSystem31N = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  ETRS89CoordinateSystem31N: TETRS89CoordinateSystem31N;

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
  Result := GeodeticToCartesian(Coordinates, GRS80Ellipsoid);
End;

Function TETRS89CoordinateSystemGD.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := CartesianToGeodetic(Coordinates, GRS80Ellipsoid);
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

ETRS89CoordinateSystemGC.Initialize('ETRS89 Geocentric', 'ETRS89GC', 'ETRS89 Geocentric',
                                    4936, ctGeocentric, aoXYZ);
ETRS89CoordinateSystemGD.Initialize('ETRS89 Geodetic', 'ETRS89GD', 'ETRS89 Geodetic',
                                    4937, ctGeodetic, aoYXZ);
ETRS89CoordinateSystem31N.Initialize('ETRS89 UTM 31N', 'UTM31N', 'ETRS89 / UTM Zone 31N',
                                     25831, ctCartesian, aoXYZ);
ETRS89CoordinateSystem30N.Initialize('ETRS89 UTM 30N', 'UTM30N', 'ETRS89 / UTM Zone 30N',
                                     25830, ctCartesian, aoXYZ);
ETRS89CoordinateSystem29N.Initialize('ETRS89 UTM 29N', 'UTM29N', 'ETRS89 / UTM Zone 29N',
                                     25829, ctCartesian, aoXYZ);
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
CoordinateSystems.Register(ETRS89CoordinateSystemGC);
CoordinateSystems.Register(ETRS89CoordinateSystemGD);
CoordinateSystems.Register(ETRS89CoordinateSystem29N);
CoordinateSystems.Register(ETRS89CoordinateSystem30N);
CoordinateSystems.Register(ETRS89CoordinateSystem31N);

End.

