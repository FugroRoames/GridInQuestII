Unit Geometry;

{ Geometry Computation Library.

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

{ Define only one of the following symbols. }
//{$DEFINE SINGLE_GEOMETRY}
{$DEFINE DOUBLE_GEOMETRY}
//{$DEFINE EXTENDED_GEOMETRY}

{ Define to uses inline optimisation. }
//{$DEFINE USE_INLINE}

Interface

Uses
  SysUtils, Math;

{$IFDEF SINGLE_GEOMETRY}
  Type
    TCoordinate = Single;

  Const
    MinCoordinate = MinSingle;
    MaxCoordinate = MaxSingle;
{$ENDIF}

{$IFDEF DOUBLE_GEOMETRY}
  Type
    TCoordinate = Double;

  Const
    MinCoordinate = MinDouble;
    MaxCoordinate = MaxDouble;
{$ENDIF}

{$IFDEF EXTENDED_GEOMETRY}
  Type
    TCoordinate = Double;

  Const
    MinCoordinate = MinExtended;
    MaxCoordinate = MaxExtended;
{$ENDIF}

Type
  TCoordinateArray = Array Of TCoordinate;

Type
  T2DCoordinates = Packed Object
    X, Y: TCoordinate;
  End;
  T2DCoordinatesArray = Array Of T2DCoordinates;
  T2DCoordinatesArrayArray = Array Of T2DCoordinatesArray;

Type
  T3DCoordinates = Packed Object(T2DCoordinates)
    Z: TCoordinate;
  End;
  T3DCoordinatesArray = Array Of T3DCoordinates;
  T3DCoordinatesArrayArray = Array Of T3DCoordinatesArray;

Type
  TCoordinates = T3DCoordinates;
  TCoordinatesArray = Array Of TCoordinates;
  TCoordinatesArrayArray = Array Of TCoordinatesArray;

Type
  TLine = Packed Record
      P1, P2: TCoordinates;
    End;

Type
  TPolyLine = Array Of TCoordinates;

Type
  TLoop = Array Of TCoordinates;

Type
  TPolygon = Array Of TLoop;

Type
  TMultiPolygon = Array Of TPolygon;

Type TExtents = Packed Object
    P1, P2: TCoordinates;
    Property Min: TCoordinates Read P1 Write P1;
    Property Max: TCoordinates Read P2 Write P2;
  End;

Const
  NullCoordinates: TCoordinates = (X: 0; Y: 0; Z: 0);

{ Coordinate formatting routines. }
Function Format2DCoordinates(Point: T2DCoordinates): String;
Function Format3DCoordinates(Point: T3DCoordinates): String;

Implementation

Function Format2DCoordinates(Point: T2DCoordinates): String;
Begin
  Result := FormatFloat('0', Point.X)+','+FormatFloat('0', Point.Y);
End;

Function Format3DCoordinates(Point: T3DCoordinates): String;
Begin
  Result := FormatFloat('0', Point.X)+','+FormatFloat('0', Point.Y)+','+FormatFloat('0', Point.Z);
End;

End.

