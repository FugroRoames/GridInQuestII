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

Type TExtents = Packed Object
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

Type
  TSexagesimalCoordinate = Packed Object
      Degrees, Minutes, Seconds, Sign: TCoordinate;
  End;

Type TSexagesimalCoordinates = Packed Object
    Latitude, Longitude: TSexagesimalCoordinate;
    Altitude: TCoordinate;
  End;

Const
  OneOverSixty: TCoordinate = 1/60;
  OneOverSixtySquared: TCoordinate = 1/(60*60);
  NullCoordinates: TCoordinates = (X: 0; Y: 0; Z: 0);

Function SexagesimalToDecimalCoordinate(Coordinate: TSexagesimalCoordinate): TCoordinate;
Function DecimalToSexagesimalCoordinate(Coordinate: TCoordinate): TSexagesimalCoordinate;
Function SexagesimalToDecimalCoordinates(Const Coordinates: TSexagesimalCoordinates): TCoordinates;
Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates): TSexagesimalCoordinates;

Type TCoordinateSystem = Object
    Abbreviation: String;
    AxisOrder: TAxisOrder;
    CoordinateType: TCoordinateType;
    Description: String;
    EPSGNumber: Integer;
    Name: String;
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
  End;

Type TCoordinateSystemPointer = ^TCoordinateSystem;

Type TCoordinateSystems = Object
    Count: Integer;
    Function AvailableSystemsList(OmitIndex: Integer = -1): String;
    Function FindByDescription(Const Description: String): Integer;
    Function FindEPSGNumber(Const Number: Integer): Integer;
    Function IndexOf(Const CoordinateSystem: TCoordinateSystem): Integer;
    Function Items(Index: Integer): TCoordinateSystem;
    Function Pointers(Index: Integer): TCoordinateSystemPointer;
    Procedure Register(Const CoordinateSystem: TCoordinateSystem);
  End;

Var
  CoordinateSystems: TCoordinateSystems;

Implementation

Function SexagesimalToDecimalCoordinate(Coordinate: TSexagesimalCoordinate): TCoordinate;
Begin;
  With Coordinate Do
    Result := Sign*(Degrees+Minutes*OneOverSixty+Seconds*OneOverSixtySquared);
End;

Function DecimalToSexagesimalCoordinate(Coordinate: TCoordinate): TSexagesimalCoordinate;
Begin
  With Result Do
    Begin
      Sign := Math.Sign(Coordinate);
      Coordinate := Abs(Coordinate);
      Degrees := Int(Coordinate);
      Coordinate := Frac(Coordinate)*60;
      Minutes := Int(Coordinate);
      Seconds := Frac(Coordinate)*60;
    End;
End;

Function SexagesimalToDecimalCoordinates(Const Coordinates: TSexagesimalCoordinates): TCoordinates;
Begin
  With Result Do
    Begin
      Latitude := SexagesimalToDecimalCoordinate(Coordinates.Latitude);
      Longitude := SexagesimalToDecimalCoordinate(Coordinates.Longitude);
      Altitude := Coordinates.Altitude;
    End;
End;

Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates): TSexagesimalCoordinates;
Begin
  With Result Do
    Begin
      Latitude := DecimalToSexagesimalCoordinate(Coordinates.Latitude);
      Longitude := DecimalToSexagesimalCoordinate(Coordinates.Longitude);
      Altitude := Coordinates.Altitude;
    End;
End;

Function TCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  { Actual conversions performed by child implementations. }
  Result := NullCoordinates;
End;

Function TCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  { Actual conversions performed by child implementations. }
  Result := NullCoordinates;
End;

Var
  CoordinateSystemsList: Array Of TCoordinateSystemPointer;

Function TCoordinateSystems.AvailableSystemsList(OmitIndex: Integer = -1): String;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Index<>OmitIndex Then
      Begin
        Result := Result+CoordinateSystemsList[Index]^.Description;
        If Index<>LastIndex Then
          Result := Result+LineEnding;
      End;
End;

Function TCoordinateSystems.FindByDescription(Const Description: String): Integer;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Description=CoordinateSystemsList[Index]^.Description Then
      Begin
        Result := Index;
        Exit;
      End;
  Result := -1;
End;

Function TCoordinateSystems.FindEPSGNumber(Const Number: Integer): Integer;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Number=CoordinateSystemsList[Index]^.EPSGNumber Then
      Begin
        Result := Index;
        Exit;
      End;
  Result := -1;
End;

Function TCoordinateSystems.IndexOf(Const CoordinateSystem: TCoordinateSystem): Integer;
Begin
  Result := FindEPSGNumber(CoordinateSystem.EPSGNumber);
End;

Function TCoordinateSystems.Items(Index: Integer): TCoordinateSystem;
Begin
  Result := CoordinateSystemsList[Index]^;
End;

Function TCoordinateSystems.Pointers(Index: Integer): TCoordinateSystemPointer;
Begin
  Result := CoordinateSystemsList[Index];
End;

Procedure TCoordinateSystems.Register(Const CoordinateSystem: TCoordinateSystem);
Begin
  Count := Length(CoordinateSystemsList);
  Inc(Count);
  SetLength(CoordinateSystemsList, Count);
  CoordinateSystemsList[High(CoordinateSystemsList)] := @CoordinateSystem;
End;

End.

