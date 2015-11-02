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
  SysUtils, Math, Geometry, Geodesy, OSTab;

Type
  TITMCoordinateSystem = Object(TCoordinateSystem)
    PreferredVerticalDatum: TVerticalDatumCode;
    LastVerticalDatum: TVerticalDatumCode;
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Var
  GM02NIData: TVerticalTable;
  GM15NIData: TVerticalTable;
  GM02RoIData: TVerticalTable;
  GM15RoIData: TVerticalTable;
  GRS80Ellipsoid: TEllipsoid;
  ITMProjection: TProjection;
  ITMCoordinateSystem: TITMCoordinateSystem;

Const
  ITMBounds: TGeodeticBounds = (Western: -10.56*PI/180; Southern: 51.39*PI/180; Eastern: -5.34*PI/180; Northern: 55.43*PI/180);

Function WGS84CoordinatesToITMCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Function ITMCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;

Implementation

Var
  ProgramFolder: String;
  GM02NIFileName: String;
  GM02RoIFileName: String;
  GM15NIFileName: String;
  GM15RoIFileName: String;
  GM02NIDataFound: Boolean;
  GM02RoIDataFound: Boolean;
  GM15NIDataFound: Boolean;
  GM15RoIDataFound: Boolean;

Function TITMCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  // Test for bounds?
  If ITMCoordinatesToWGS84Coordinates(Coordinates, vmGM02, PreferredVerticalDatum, GeodeticCoordinates, LastVerticalDatum) Then
    Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid)
  Else
    Result := NullCoordinates;
End;

Function TITMCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  // Test for bounds?
  If Not WGS84CoordinatesToITMCoordinates(GeodeticCoordinates, vmGM02, PreferredVerticalDatum, Result, LastVerticalDatum) Then
    Result := NullCoordinates;
End;

Function WGS84CoordinatesToITMCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Var
  NIGMData: TVerticalTable;
  RoIGMData: TVerticalTable;
  NIParameters: TInterpolationParameters;
  RoIParameters: TInterpolationParameters;
  NIValid: Boolean;
  RoIValid: Boolean;
Const
  GridScale = 1000;
  Procedure OutputNI;
  Begin
    With NIParameters Do
      OutputDatum := TVerticalDatumCode(NIGMData.Data(X1, Y1).DatumCode);
    Result := (OutputDatum<>vdNone);
    If Result Then
      OutputCoordinates.Altitude := OutputCoordinates.Altitude-InterpolateVerticalTable(NIGMData, NIParameters)
  End;
  Procedure OutputRoI;
  Begin
    With RoIParameters Do
      OutputDatum := TVerticalDatumCode(RoIGMData.Data(X1, Y1).DatumCode);
    Result := (OutputDatum<>vdNone);
    If Result Then
      OutputCoordinates.Altitude := OutputCoordinates.Altitude-InterpolateVerticalTable(RoIGMData, RoIParameters)
  End;
Begin
  Result := False;
  Case VerticalModel Of
  vmGM02:
    Begin
      NIGMData := GM02NIData;
      RoIGMData := GM02RoIData;
    End;
  vmGM15:
    Begin
      NIGMData := GM15NIData;
      RoIGMData := GM15RoIData;
    End;
  End;
  OutputCoordinates := TransverseMercator(InputCoordinates, ITMProjection);
  NIParameters := BilinearGridInterpolationParameters(NIGMData.Header.Origin, OutputCoordinates, GridScale);
  NIValid := ParametersValid(NIParameters, NIGMData.Header);
  RoIParameters := BilinearGridInterpolationParameters(RoIGMData.Header.Origin, OutputCoordinates, GridScale);
  RoIValid := ParametersValid(RoIParameters, RoIGMData.Header);
  If NIValid Then
    Begin
      { If both data tables are valid. }
      If RoIValid Then
        Begin
          { If NI preferred then try it first. }
          If PreferredDatum=vdBelfast Then
            OutputNI;
          { If there was no valid output, try RoI. }
          If Not Result Then
            OutputRoI;
          { If still no valid output, try NI again if needed. }
          If Not Result And (PreferredDatum<>vdBelfast) Then
            OutputNI;
        End
      Else
        OutputNI;
    End
  Else
    Begin
      If RoIValid Then
        OutputRoI;
    End;
End;

Function ITMCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Var
  NIGMData: TVerticalTable;
  RoIGMData: TVerticalTable;
  NIParameters: TInterpolationParameters;
  RoIParameters: TInterpolationParameters;
  NIValid: Boolean;
  RoIValid: Boolean;
  Procedure OutputNI;
  Begin
    With NIParameters Do
      OutputDatum := TVerticalDatumCode(NIGMData.Data(X1, Y1).DatumCode);
    Result := (OutputDatum<>vdNone);
    If Result Then
      OutputCoordinates.Altitude := OutputCoordinates.Altitude+InterpolateVerticalTable(NIGMData, NIParameters)
  End;
  Procedure OutputRoI;
  Begin
    With RoIParameters Do
      OutputDatum := TVerticalDatumCode(RoIGMData.Data(X1, Y1).DatumCode);
    Result := (OutputDatum<>vdNone);
    If Result Then
      OutputCoordinates.Altitude := OutputCoordinates.Altitude+InterpolateVerticalTable(RoIGMData, RoIParameters)
  End;
Const
  GridScale = 1000;
Begin
  Result := False;
  Case VerticalModel Of
  vmGM02:
    Begin
      NIGMData := GM02NIData;
      RoIGMData := GM02RoIData;
    End;
  vmGM15:
    Begin
      NIGMData := GM15NIData;
      RoIGMData := GM15RoIData;
    End;
  End;
  NIParameters := BilinearGridInterpolationParameters(NIGMData.Header.Origin, InputCoordinates, GridScale);
  NIValid := ParametersValid(NIParameters, NIGMData.Header);
  RoIParameters := BilinearGridInterpolationParameters(RoIGMData.Header.Origin, InputCoordinates, GridScale);
  RoIValid := ParametersValid(RoIParameters, RoIGMData.Header);
  OutputCoordinates := InverseTransverseMercator(InputCoordinates, ITMProjection);
  If NIValid Then
    Begin
      { If both data tables are valid. }
      If RoIValid Then
        Begin
          { If NI preferred then try it first. }
          If PreferredDatum=vdBelfast Then
            OutputNI;
          { If there was no valid output, try RoI. }
          If Not Result Then
            OutputRoI;
          { If still no valid output, try NI again if needed. }
          If Not Result And (PreferredDatum<>vdBelfast) Then
            OutputNI;
        End
      Else
        OutputNI;
    End
  Else
    Begin
      If RoIValid Then
        OutputRoI;
    End;
End;

Initialization

ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
GM02NIFileName := ProgramFolder+'GM02NI.dat';
GM02RoIFileName := ProgramFolder+'GM02RoI.dat';
GM15NIFileName := ProgramFolder+'GM15NI.dat';
GM15RoIFileName := ProgramFolder+'GM15RoI.dat';
GM02NIDataFound := GM02NIData.LoadFromFile(GM02NIFileName);
GM02RoIDataFound := GM02RoIData.LoadFromFile(GM02RoIFileName);
GM15NIDataFound := GM15NIData.LoadFromFile(GM15NIFileName);
GM15RoIDataFound := GM15RoIData.LoadFromFile(GM15RoIFileName);
GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
ITMProjection.Initialize(0.99982, DegToRad(53.5), DegToRad(-8), 600000, 750000, GRS80Ellipsoid);
ITMCoordinateSystem.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'Irish Transverse Mercator (ITM)', 2157, ctProjected, aoXYZ, ITMBounds);
ITMCoordinateSystem.PreferredVerticalDatum := vdMalinHead;
CoordinateSystems.Register(ITMCoordinateSystem);

End.

