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
  Math, Geometry, Geodesy;

Type TITMCoordinateSystem95 = Object(TCoordinateSystem)
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Var
  ITMCoordinateSystem95: TITMCoordinateSystem95 =
    (
      Abbreviation: 'IRENET95';
      AxisOrder: aoXYZ;
      CoordinateType: ctCartesian;
      Description: 'IRENET95 / Irish Transverse Mercator (ITM/VRF10)';
      EPSGNumber: 2157;
      Name: 'Irish Transverse Mercator';
    );

Implementation

Function TITMCoordinateSystem95.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Function TITMCoordinateSystem95.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  Result := NullCoordinates;
End;

Initialization

CoordinateSystems.Register(ITMCoordinateSystem95);

End.

