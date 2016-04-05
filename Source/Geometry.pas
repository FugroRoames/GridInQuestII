Unit Geometry;

{ Geometry Computation Library.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
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
  Math;

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

Type
  TExtents = Packed Object
    P1, P2: TCoordinates;
    Property Min: TCoordinates Read P1 Write P1;
    Property Max: TCoordinates Read P2 Write P2;
  End;

Const
  OneOverOneThousand: TCoordinate = 1/1000;
  OneOverTenThousand: TCoordinate = 1/10000;
  OneOverOneHundredThousand: TCoordinate = 1/100000;
  OneOverFiveHundredThousand: TCoordinate = 1/500000;
  OneOverOneMillion: TCoordinate = 1/1000000;
  ConvergenceThreshold: TCoordinate = 0.00001;
  NullCoordinates: TCoordinates = (X: 0; Y: 0; Z: 0);
  TwoPI: TCoordinate = 2*Pi;
  HalfPi: TCoordinate = Pi/2;

{ Coordinates operator overloads. }
Operator = (A, B: T2DCoordinates): Boolean;
Operator = (A, B: T3DCoordinates): Boolean;
Operator := (A: T2DCoordinates): T3DCoordinates;
Operator + (A, B: T2DCoordinates): T2DCoordinates;
Operator + (A, B: T3DCoordinates): T3DCoordinates;
Operator - (A, B: T2DCoordinates): T2DCoordinates;
Operator - (A, B: T3DCoordinates): T3DCoordinates;

{ Geometry functions. }
Function NormalizeAngle(Angle: TCoordinate): TCoordinate; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Function WithinExtents(P: TCoordinates; E: TExtents): Boolean; {$IFDEF USE_INLINE}Inline;{$ENDIF}

Implementation

Operator = (A, B: T2DCoordinates): Boolean;
Begin
  Result := (A.X=B.X) And (A.Y=B.Y);
End;

Operator = (A, B: T3DCoordinates): Boolean;
Begin
  Result := (A.X=B.X) And (A.Y=B.Y) And (A.Z=B.Z);
End;

Operator := (A: T2DCoordinates): T3DCoordinates;
Begin
  Result.X := A.X;
  Result.Y := A.Y;
  Result.Z := 0.0;
End;

Operator+(A, B: T2DCoordinates): T2DCoordinates;
Begin
  Result.X := A.X+B.X;
  Result.Y := A.Y+B.Y;
End;

Operator+(A, B: T3DCoordinates): T3DCoordinates;
Begin
  Result.X := A.X+B.X;
  Result.Y := A.Y+B.Y;
  Result.Z := A.Z+B.Z;
End;

Operator-(A, B: T2DCoordinates): T2DCoordinates;
Begin
  Result.X := A.X-B.X;
  Result.Y := A.Y-B.Y;
End;

Operator-(A, B: T3DCoordinates): T3DCoordinates;
Begin
  Result.X := A.X-B.X;
  Result.Y := A.Y-B.Y;
  Result.Z := A.Z-B.Z;
End;

Function NormalizeAngle(Angle: TCoordinate): TCoordinate; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Begin
  Result := Angle;
  While Result>TwoPI Do
    Result -= TwoPI;
  While Result<0 Do
    Result += TwoPI;
End;

Function WithinExtents(P: TCoordinates; E: TExtents): Boolean; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Begin
  If P.X<E.Max.X Then
    If P.Y<E.Max.Y Then
      If P.Z<E.Max.Z Then
        If P.X>=E.Min.X Then
          If P.Y>=E.Min.Y Then
            If P.Z>=E.Min.Z Then
              Begin
                Result := True;
                Exit;
              End;
  Result := False;
End;

End.

