Unit ITM;

{ Irish Transverse Mercator Unit.

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
  Math, Geometry, Geodesy, OSMod;

Type TITMCoordinateSystem95 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GRS80Ellipsoid: TEllipsoid;
  ITMProjection: TProjection;
  ITMCoordinateSystem95: TITMCoordinateSystem95;

Implementation

Function TITMCoordinateSystem95.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := InverseTransverseMercator(Coordinates, ITMProjection);
  Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid);
End;

Function TITMCoordinateSystem95.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  Result := TransverseMercator(GeodeticCoordinates, ITMProjection);
End;

Initialization

GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
ITMProjection.Initialize(0.99982, DegToRad(53.5), DegToRad(-8), 600000, 750000, GRS80Ellipsoid);
ITMCoordinateSystem95.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'IRENET95 / Irish Transverse Mercator (ITM/VRF10)',
                                  2157, ctCartesian, aoXYZ);
CoordinateSystems.Register(ITMCoordinateSystem95);

End.

