Unit GeomUtils;

{ Geometry Formatting Utilities Library.

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
  Classes, SysUtils, StrUtils, Math, Geometry;

Type
  TUnitType = (utLength, utArea);

Type
  TUnitSystem = (usMetric, usImperial);

Const
  MetersPerInch = 0.0254;
  MetersPerFoot = MetersPerInch * 12;
  MetersPerYard = MetersPerFoot * 3;
  MetersPerMile = MetersPerFoot * 5280;
  FeetPerMeter = 1/MetersPerFoot;
  YardsPerMeter = 1/MetersPerYard;
  MilesPerMeter = 1/MetersPerMile;
  SquareMetersPerSquareInch = MetersPerInch * MetersPerInch;
  SquareMetersPerSquareFoot = MetersPerFoot * MetersPerFoot;
  SquareMetersPerSquareYard = MetersPerYard * MetersPerYard;
  SquareMetersPerSquareMile = MetersPerMile * MetersPerMile;
  SquareMetersPerAcre = SquareMetersPerSquareYard * 4840;
  SquareYardPerSquareMeter = 1/SquareMetersPerSquareYard;
  AcresPerSquareMeter = 1/SquareMetersPerAcre;
  SquareMilesPerSquareMeter = 1/SquareMetersPerSquareMile;

Procedure DetermineOptimalCoordinateUnits(Value: TCoordinate; UnitType: TUnitType; UnitSystem: TUnitSystem; Var UnitSuffix: String; Var ValueMultiplier: TCoordinate; Var RequiredDecimalPlaces: Integer);
Function FormatCoordinate(Const Value: TCoordinate; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String; Overload;
Function FormatCoordinateWithUnits(Const Value: TCoordinate; Units: String; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String;
Function FormatCoordinateWithConversion(Const Value: TCoordinate; UnitType: TUnitType; UnitSystem: TUnitSystem = usMetric; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String; Overload;
Function FormatScaleValue(Const Scale: TCoordinate): String;
Function ExtractScaleValue(Const ScaleText: String): TCoordinate;
Function FormatCoordinatePair(Const Coordinates: TCoordinates; DecimalPlaces: Integer = -1): String;
Function ExtractCoordinatePair(Const CoordinatesText: String; Out Coordinates: TCoordinates): Boolean;

Implementation

Procedure DetermineOptimalCoordinateUnits(Value: TCoordinate; UnitType: TUnitType; UnitSystem: TUnitSystem; Var UnitSuffix: String; Var ValueMultiplier: TCoordinate; Var RequiredDecimalPlaces: Integer);
Begin
  If UnitSystem=usMetric Then
    If UnitType=utLength Then
      If Value<1 Then
        Begin
          UnitSuffix := 'cm';
          ValueMultiplier := 100;
        End
      Else If Value<1000 Then
        Begin
          UnitSuffix := 'm';
          ValueMultiplier := 1;
        End
      Else
        Begin
          UnitSuffix := 'Km';
          ValueMultiplier := OneOverOneThousand;
        End
    Else
      If Value<10000 Then
        Begin
          UnitSuffix := 'sqm';
          ValueMultiplier := 1;
        End
      Else If Value<1000000 Then
        Begin
          UnitSuffix := 'Ha';
          ValueMultiplier := OneOverTenThousand;
        End
      Else
        Begin
          UnitSuffix := 'sqKm';
          ValueMultiplier := OneOverOneMillion;
        End
  Else
    If UnitType=utLength Then
      If Value<MetersPerYard Then
        Begin
          UnitSuffix := 'ft';
          ValueMultiplier := FeetPerMeter;
        End
      Else If Value<MetersPerMile Then
        Begin
          UnitSuffix := 'yd';
          ValueMultiplier := YardsPerMeter;
        End
      Else
        Begin
          UnitSuffix := 'mi';
          ValueMultiplier := MilesPerMeter;
        End
    Else
      If Value<SquareMetersPerAcre Then
        Begin
          UnitSuffix := 'sqyd';
          ValueMultiplier := SquareYardPerSquareMeter;
        End
      Else If Value<SquareMetersPerSquareMile Then
        Begin
          UnitSuffix := 'acr';
          ValueMultiplier := AcresPerSquareMeter;
        End
      Else
        Begin
          UnitSuffix := 'sqmi';
          ValueMultiplier := SquareMilesPerSquareMeter;
        End;
  Value := Abs(Value*ValueMultiplier);
  If Value >10 Then
    RequiredDecimalPlaces := 0
  Else
    If Value=0 Then
      RequiredDecimalPlaces := 0
    Else
      RequiredDecimalPlaces := Trunc(1+Abs(Log10(Value)));
End;

Function FormatCoordinate(Const Value: TCoordinate; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String; Overload;
Var
  PlaceText: String;
Begin
  If DecimalPlaces=-1 Then
    Result := FloatToStr(Value)
  Else
    Begin
      If ZeroFill Then
        PlaceText := StringOfChar('0', DecimalPlaces)
      Else
        PlaceText := StringOfChar('#', DecimalPlaces);
      Result := FormatFloat('0.'+PlaceText, Value);
      { Prevent the '-0.00' type output for very small negative values. }
      If (StrToFloat(Result)=0) And (Result[1]='-') Then
        Result := Copy(Result, 2, Length(Result));
    End;
End;

Function FormatCoordinateWithUnits(Const Value: TCoordinate; Units: String; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String;
Begin
  Result := FormatCoordinate(Value, DecimalPlaces, ZeroFill)+Units;
End;

Function FormatCoordinateWithConversion(Const Value: TCoordinate; UnitType: TUnitType; UnitSystem: TUnitSystem = usMetric; DecimalPlaces: Integer = -1; ZeroFill: Boolean = False): String; Overload;
Var
  AdjustedValue: TCoordinate;
  CoordinateFormat: String;
  Units: String;
  ValueMultiplier: TCoordinate;
  RequiredDecimalPlaces: Integer;
Begin
  DetermineOptimalCoordinateUnits(Value, UnitType, UnitSystem, Units, ValueMultiplier, RequiredDecimalPlaces);
  AdjustedValue := Value*ValueMultiplier;
  If DecimalPlaces=-1 Then
    Result := FormatCoordinateWithUnits(AdjustedValue, Units, RequiredDecimalPlaces, ZeroFill)
  Else
    Result := FormatCoordinateWithUnits(AdjustedValue, Units, DecimalPlaces, ZeroFill);
End;

Function FormatScaleValue(Const Scale: TCoordinate): String;
Begin
  Result := '1:'+FloatToStrF(Trunc(Scale), ffNumber, 15, 0);
End;

Function ExtractScaleValue(Const ScaleText: String): TCoordinate;
Var
  Index: Integer;
  ValueText: String;
Begin
  Index := Pos(':', ScaleText)+1;
  ValueText := Copy(ScaleText, Index, Length(ScaleText));
  Index := Pos(ThousandSeparator, ValueText);
  While (Index<>0) Do
    Begin
      Delete(ValueText, Index, 1);
      Index := Pos(ThousandSeparator, ValueText);
    End;
  Val(ValueText, Result, Index);
  If Index<>0 Then
    Result := 0;
End;

Function FormatCoordinatePair(Const Coordinates: TCoordinates; DecimalPlaces: Integer = -1): String;
Begin
  With Coordinates Do
    Result := FormatCoordinate(X, DecimalPlaces)+','+FormatCoordinate(Y, DecimalPlaces);
End;

Function ExtractCoordinatePair(Const CoordinatesText: String; Out Coordinates: TCoordinates): Boolean;
Var
  SplitIndex: Integer;
Begin
  SplitIndex := Pos(',', CoordinatesText);
  With Coordinates Do
    Begin
      X := StrToFloatDef(Copy(CoordinatesText, 1, SplitIndex-1),0);
      Y := StrToFloatDef(Copy(CoordinatesText, SplitIndex+1, Length(CoordinatesText)),0);
      Z := 0;
    End;
  Result := (Coordinates<>NullCoordinates);
End;

End.

