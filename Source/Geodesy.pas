Unit Geodesy;

{ Geodesy Computation Library.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details. }

{ Usage Note:
  Geodesy must follow Geometry on the Uses clause for the redefinition of
  TCoordinates to be visible in code. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Math, Geometry;

Type
  TCoordinateType = (ctGeocentric, ctGeodetic, ctCartesian);

Type
  TAxisOrder = (aoXYZ, aoYXZ);

Type
  TPlanarCoordinates = Packed Object(Geometry.T2DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
  End;

Type
  TCoordinates = Packed Object(Geometry.T3DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Latitude: TCoordinate Read Y Write Y;
    Property Longitude: TCoordinate Read X Write X;
    Property Altitude: TCoordinate Read Z Write Z;
  End;

Type
  TMeasuredCoordinates = Packed Object(Geometry.T3DCoordinates)
    M: TCoordinate;
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Measure: TCoordinate Read M Write M;
  End;

Type TExtents = Packed Object//(Geometry.TExtents)
    P1, P2: TCoordinates;
    Property Min: TCoordinates Read P1 Write P1;
    Property Max: TCoordinates Read P2 Write P2;
    Property SouthWest: TCoordinates Read P1 Write P1;
    Property NorthEast: TCoordinates Read P2 Write P2;
    Property Western: TCoordinate Read P1.X Write P1.X;
    Property Southern: TCoordinate Read P1.Y Write P1.Y;
    Property Eastern: TCoordinate Read P2.X Write P2.X;
    Property Northern: TCoordinate Read P2.Y Write P2.Y;
  End;

Function GetAvailableSystemsList: String;

Implementation

Function GetAvailableSystemsList: String;
Begin
  Result := 'ETRS89 Geocentric'#13#10+
            'ETRS89 Geodetic'#13#10+
            'ETRS89 / UTM Zone 29N'#13#10+
            'ETRS89 / UTM Zone 30N'#13#10+
            'ETRS89 / UTM Zone 31N'#13#10+
            'IRENET95 / Irish Transverse Mercator (ITM)'#13#10+
            'OSGB36 / British National Grid (BNG)'#13#10+
            'TM75 / Irish Grid (IG)';
End;

{
ETRS89 Geocentric
ETRS89 Geodetic
ETRS89 / UTM Zone 29N
ETRS89 / UTM Zone 30N
ETRS89 / UTM Zone 31N
IRENET95 / Irish Transverse Mercator (ITM/VRF10)
OSGB36 / British National Grid (BNG/GM02) //?????
OSGB36 / British National Grid (BNG/VRF10)
OSGB36 Geocentric (OSNET) //?????
TM75 / Irish Grid (IG)
}

End.

