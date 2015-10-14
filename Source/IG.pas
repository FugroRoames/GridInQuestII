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
  Math, Geometry, Geodesy;

Type TIGCoordinateSystem75 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  Airy1830ModifiedEllipsoid: TEllipsoid;
  IrishGridProjection: TProjection;
  IGCoordinateSystem75: TIGCoordinateSystem75;

Implementation

Function TIGCoordinateSystem75.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := InverseTransverseMercator(Coordinates, IrishGridProjection);
  Result := GeodeticToGeocentric(GeodeticCoordinates, Airy1830ModifiedEllipsoid);
End;

Function TIGCoordinateSystem75.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, Airy1830ModifiedEllipsoid);
  Result := TransverseMercator(GeodeticCoordinates, IrishGridProjection);
End;

Initialization

Airy1830ModifiedEllipsoid.Initialize(6377340.1890, 6356034.4470);
IrishGridProjection.Initialize(1.000035, DegToRad(53.5), DegToRad(-8), 200000, 250000, Airy1830ModifiedEllipsoid);
IGCoordinateSystem75.Initialize('Irish Grid (1975)', 'TM75IG', 'TM75 / Irish Grid (IG)',
                                29903, ctCartesian, aoXYZ);
CoordinateSystems.Register(IGCoordinateSystem75);

End.

