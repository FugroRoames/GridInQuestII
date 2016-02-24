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
  Math, Geodesy;

Type
  TETRS89CoordinateSystemGC = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type
  TETRS89CoordinateSystemGD = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type
  TETRS89CoordinateSystemUTM = Object(TCoordinateSystem)
    ProjectionPointer: TProjectionPointer;
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewSRIDNumber: Integer;
                           NewRevision: Integer; NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                           NewBounds: TGeodeticBounds; Const NewProjection: TProjection);
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GRS80Ellipsoid: TEllipsoid;
  UTMZone29Projection: TProjection;
  UTMZone30Projection: TProjection;
  UTMZone31Projection: TProjection;
  ETRS89CoordinateSystemGC: TETRS89CoordinateSystemGC;
  ETRS89CoordinateSystemGD: TETRS89CoordinateSystemGD;
  ETRS89CoordinateSystem29N: TETRS89CoordinateSystemUTM;
  ETRS89CoordinateSystem30N: TETRS89CoordinateSystemUTM;
  ETRS89CoordinateSystem31N: TETRS89CoordinateSystemUTM;

Const
  EuropeBounds: TGeodeticBounds = (Western: -16.1*PI/180; Southern: 32.88*PI/180; Eastern: 39.65*PI/180; Northern: 84.17*PI/180);
  UTM31NBounds: TGeodeticBounds = (Western: 0; Southern: 0; Eastern: 6*PI/180; Northern: 84*PI/180);
  UTM30NBounds: TGeodeticBounds = (Western: -6*PI/180; Southern: 0; Eastern: 0; Northern: 84*PI/180);
  UTM29NBounds: TGeodeticBounds = (Western: -12*PI/180; Southern: 0; Eastern: -6*PI/180; Northern:  84*PI/180);

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
  Result := GeodeticToGeocentric(Coordinates, GRS80Ellipsoid);
End;

Function TETRS89CoordinateSystemGD.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
End;

Constructor TETRS89CoordinateSystemUTM.Initialize(NewName: String;
  NewAbbreviation: String; NewDescription: String; NewSRIDNumber: Integer; NewRevision: Integer;
  NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
  NewBounds: TGeodeticBounds; Const NewProjection: TProjection);
Begin
  Inherited Initialize(NewName, NewAbbreviation, NewDescription, NewSRIDNumber, NewRevision,
                             NewCoordinateType, NewAxisOrder, NewBounds);
  ProjectionPointer := @NewProjection;
End;

Function TETRS89CoordinateSystemUTM.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := InverseTransverseMercator(Coordinates, ProjectionPointer^);
  If WithinGeodeticBounds(GeodeticCoordinates) Then
    Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid)
  Else
    Result := NullCoordinates
End;

Function TETRS89CoordinateSystemUTM.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  If WithinGeodeticBounds(GeodeticCoordinates) Then
    Result := TransverseMercator(GeodeticCoordinates, ProjectionPointer^)
  Else
    Result := NullCoordinates;
End;

Initialization

GRS80Ellipsoid.Initialize(6378137.0000, 6356752.3141);
UTMZone29Projection.Initialize(0.9996, 0, DegToRad(-9), 500000, 0, GRS80Ellipsoid);
UTMZone30Projection.Initialize(0.9996, 0, DegToRad(-3), 500000, 0, GRS80Ellipsoid);
UTMZone31Projection.Initialize(0.9996, 0, DegToRad(3), 500000, 0, GRS80Ellipsoid);
ETRS89CoordinateSystemGC.Initialize('ETRS89 Cartesian', 'ETRS89CT', 'ETRS89 Cartesian', 4936, 1989, ctCartesian, aoXYZ, EuropeBounds);
ETRS89CoordinateSystemGD.Initialize('ETRS89 Geodetic', 'ETRS89GD', 'ETRS89 Geodetic', 4937, 1989, ctGeodetic, aoYXZ, EuropeBounds);
ETRS89CoordinateSystem29N.Initialize('ETRS89 UTM 29N', 'UTM29N', 'ETRS89 / UTM Zone 29N', 25829, 1989, ctProjected, aoXYZ, UTM29NBounds, UTMZone29Projection);
ETRS89CoordinateSystem30N.Initialize('ETRS89 UTM 30N', 'UTM30N', 'ETRS89 / UTM Zone 30N', 25830, 1989, ctProjected, aoXYZ, UTM30NBounds, UTMZone30Projection);
ETRS89CoordinateSystem31N.Initialize('ETRS89 UTM 31N', 'UTM31N', 'ETRS89 / UTM Zone 31N', 25831, 1989, ctProjected, aoXYZ, UTM31NBounds, UTMZone31Projection);
CoordinateSystems.Register(ETRS89CoordinateSystemGC);
CoordinateSystems.Register(ETRS89CoordinateSystemGD);
CoordinateSystems.Register(ETRS89CoordinateSystem29N);
CoordinateSystems.Register(ETRS89CoordinateSystem30N);
CoordinateSystems.Register(ETRS89CoordinateSystem31N);

End.

