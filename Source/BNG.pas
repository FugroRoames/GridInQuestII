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
  Math, Geometry, Geodesy;

Type TBNGCoordinateSystem02 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  BNGCoordinateSystem02: TBNGCoordinateSystem02 =
    (
      Abbreviation: 'OSGB36';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'OSGB36 / British National Grid (BNG/GM02)';
      EPSGNumber: 27700;
      Name: 'British National Grid (2002)';
    );

Type TBNGCoordinateSystem10 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  BNGCoordinateSystem10: TBNGCoordinateSystem10 =
    (
      Abbreviation: 'OSGB36';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'OSGB36 / British National Grid (BNG/VRF10)';
      EPSGNumber: 27700;
      Name: 'British National Grid (2010)';
    );

Implementation

Function TBNGCoordinateSystem02.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TBNGCoordinateSystem02.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TBNGCoordinateSystem10.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TBNGCoordinateSystem10.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Initialization

CoordinateSystems.Register(BNGCoordinateSystem02);
CoordinateSystems.Register(BNGCoordinateSystem10);

End.

