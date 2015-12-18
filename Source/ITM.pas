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
  SysUtils, Math, Geodesy, OSTab;

{ Define to embed the data table within the executable. }
{$DEFINE EMBED}

{ Define to include ITM using GM02 as an additional coordinate system. }
//{$DEFINE ITM02}

Type
  TITMCoordinateSystem = Object(TCoordinateSystem)
//    PreferredVerticalDatum: TVerticalDatumCode; { These fields should be define here but have been moved to the parent object in the Geodesy unit. }
//    LastVerticalDatum: TVerticalDatumCode; { They have been moved to avoid a compiler bug that incorrectly handles object inheritance. }
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                           NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                           NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode);
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
  ITM02CoordinateSystem: TITMCoordinateSystem;
  ITM15CoordinateSystem: TITMCoordinateSystem;

Const
  ITMBounds: TGeodeticBounds = (Western: -10.56*PI/180; Southern: 51.39*PI/180; Eastern: -5.34*PI/180; Northern: 55.43*PI/180);

Function WGS84CoordinatesToITMCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Function ITMCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;

Implementation

{ Embed the required data tables using resource files. }
{$IFDEF EMBED}
{$IFDEF Darwin}
  {$IFDEF ITM02}
    {$R GM02NI.res}
    {$R GM02RoI.res}
  {$ENDIF}
  {$R TN15NI.res}
  {$R GM15RoI.res}
{$ELSE}
  {$IFDEF ITM02}
    {$R GM02NI.rc}
    {$R GM02RoI.rc}
  {$ENDIF}
  {$R GM15NI.rc}
  {$R GM15RoI.rc}
{$ENDIF}
{$ENDIF}

Type
  TAdjustDirection = (adAdd, adSubtract);
  TDatumZone = (dzNI, dzRoI);

Type
  TTransformationData = Object
    NIGMData: TVerticalTable;
    RoIGMData: TVerticalTable;
    NIParameters: TInterpolationParameters;
    RoIParameters: TInterpolationParameters;
    NIValid: Boolean;
    RoIValid: Boolean;
    PreferredDatum: TVerticalDatumCode;
    Procedure SetTables(Const VerticalModel: TOSVerticalModel);
    Procedure SetParameters(Const Coordinates: TCoordinates);
    Function SetZoneHeight(Const DatumZone: TDatumZone; Var OutputCoordinates: TCoordinates; Var OutputDatum: TVerticalDatumCode; Const AdjustDirection: TAdjustDirection): Boolean;
    Function AdjustHeight(Var OutputCoordinates: TCoordinates; Var OutputDatum: TVerticalDatumCode; Const AdjustDirection: TAdjustDirection): Boolean;
  End;

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

Const
  GridScale = 1000;

Constructor TITMCoordinateSystem.Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                                            NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                                            NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode);
Begin
  Inherited Initialize(NewName, NewAbbreviation, NewDescription, NewEPSGNumber,
                             NewCoordinateType, NewAxisOrder, NewBounds);
  PreferredVerticalDatum := NewPreferredVerticalDatum;
  LastVerticalDatum := vdNone;
End;

Function TITMCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  If ITMCoordinatesToWGS84Coordinates(Coordinates, vmGM15, PreferredVerticalDatum, GeodeticCoordinates, LastVerticalDatum) Then
    If WithinGeodeticBounds(GeodeticCoordinates) Then
      Result := GeodeticToGeocentric(GeodeticCoordinates, GRS80Ellipsoid)
    Else
      Begin
        LastVerticalDatum := vdNone;
        Result := NullCoordinates
      End;
End;

Function TITMCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  GeodeticCoordinates := GeocentricToGeodetic(Coordinates, GRS80Ellipsoid);
  If WithinGeodeticBounds(GeodeticCoordinates) Then
    If WGS84CoordinatesToITMCoordinates(GeodeticCoordinates, vmGM15, PreferredVerticalDatum, Result, LastVerticalDatum) Then
      Exit;
  LastVerticalDatum := vdNone;
  Result := NullCoordinates
End;

Procedure TTransformationData.SetTables(Const VerticalModel: TOSVerticalModel);
Begin
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
End;

Procedure TTransformationData.SetParameters(Const Coordinates: TCoordinates);
Begin
  NIParameters := BilinearGridInterpolationParameters(NIGMData.Header.Origin, Coordinates, GridScale);
  NIValid := ParametersValid(NIParameters, NIGMData.Header);
  RoIParameters := BilinearGridInterpolationParameters(RoIGMData.Header.Origin, Coordinates, GridScale);
  RoIValid := ParametersValid(RoIParameters, RoIGMData.Header);
End;

Function TTransformationData.SetZoneHeight(Const DatumZone: TDatumZone; Var OutputCoordinates: TCoordinates; Var OutputDatum: TVerticalDatumCode; Const AdjustDirection: TAdjustDirection): Boolean;
Begin
  Case DatumZone Of
  dzNI:
    Begin
      With NIParameters Do
        OutputDatum := TVerticalDatumCode(NIGMData.Data(X1, Y1).DatumCode);
      Result := (OutputDatum<>vdNone);
      If Result Then
        Case AdjustDirection Of
        adAdd:
          OutputCoordinates.Altitude := OutputCoordinates.Altitude+InterpolateVerticalTable(NIGMData, NIParameters);
        adSubtract:
          OutputCoordinates.Altitude := OutputCoordinates.Altitude-InterpolateVerticalTable(NIGMData, NIParameters);
        End;
    End;
  dzRoI:
    Begin
      With RoIParameters Do
        OutputDatum := TVerticalDatumCode(RoIGMData.Data(X1, Y1).DatumCode);
      Result := (OutputDatum<>vdNone);
      If Result Then
        Case AdjustDirection Of
        adAdd:
          OutputCoordinates.Altitude := OutputCoordinates.Altitude+InterpolateVerticalTable(RoIGMData, RoIParameters);
        adSubtract:
          OutputCoordinates.Altitude := OutputCoordinates.Altitude-InterpolateVerticalTable(RoIGMData, RoIParameters);
        End;
    End;
  End;
End;

Function TTransformationData.AdjustHeight(Var OutputCoordinates: TCoordinates; Var OutputDatum: TVerticalDatumCode; Const AdjustDirection: TAdjustDirection): Boolean;
Begin
  Result := False;
  If NIValid Then
    Begin
      { If both data tables are valid. }
      If RoIValid Then
        Begin
          { If NI preferred then try it first. }
          If PreferredDatum=vdBelfast Then
            Result := SetZoneHeight(dzNI, OutputCoordinates, OutputDatum, AdjustDirection);
          { If there was no valid output, try RoI. }
          If Not Result Then
            Result := SetZoneHeight(dzRoI, OutputCoordinates, OutputDatum, AdjustDirection);
          { If still no valid output, try NI again if needed. }
          If Not Result And (PreferredDatum<>vdBelfast) Then
            Result := SetZoneHeight(dzNI, OutputCoordinates, OutputDatum, AdjustDirection);
        End
      Else
        Result := SetZoneHeight(dzNI, OutputCoordinates, OutputDatum, AdjustDirection);
    End
  Else
    Begin
      If RoIValid Then
        Result := SetZoneHeight(dzRoI, OutputCoordinates, OutputDatum, AdjustDirection);
    End;
End;

Function WGS84CoordinatesToITMCoordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Var
  Data: TTransformationData;
Begin
  Result := False;
  Data.PreferredDatum := PreferredDatum;
  Data.SetTables(VerticalModel);
  OutputCoordinates := TransverseMercator(InputCoordinates, ITMProjection);
  Data.SetParameters(OutputCoordinates);
  Result := Data.AdjustHeight(OutputCoordinates, OutputDatum, adSubtract);
End;

Function ITMCoordinatesToWGS84Coordinates(Const InputCoordinates: TCoordinates; Const VerticalModel: TOSVerticalModel; Const PreferredDatum: TVerticalDatumCode; Out OutputCoordinates: TCoordinates; Out OutputDatum: TVerticalDatumCode): Boolean;
Var
  Data: TTransformationData;
Begin
  Result := False;
  Data.PreferredDatum := PreferredDatum;
  Data.SetTables(VerticalModel);
  Data.SetParameters(InputCoordinates);
  OutputCoordinates := InverseTransverseMercator(InputCoordinates, ITMProjection);
  Result := Data.AdjustHeight(OutputCoordinates, OutputDatum, adAdd);
End;

Initialization

ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
GM02NIFileName := ProgramFolder+'GM02NI.dat';
GM02RoIFileName := ProgramFolder+'GM02RoI.dat';
GM15NIFileName := ProgramFolder+'GM15NI.dat';
GM15RoIFileName := ProgramFolder+'GM15RoI.dat';

{ Prepare any required data table structures. }
{$IFDEF ITM02}
GM02NIData.Initialize;
GM02RoIData.Initialize;
{$ENDIF}
GM15NIData.Initialize;
GM15RoIData.Initialize;

{ Attempt to load the data tables from external files. }
{$IFDEF ITM02}
GM02NIDataFound := GM02NIData.LoadFromFile(GM02NIFileName);
GM02RoIDataFound := GM02RoIData.LoadFromFile(GM02RoIFileName);
{$ENDIF}
GM15NIDataFound := GM15NIData.LoadFromFile(GM15NIFileName);
GM15RoIDataFound := GM15RoIData.LoadFromFile(GM15RoIFileName);

{ If no external data files found, load the tables from the embedded resources. }
{$IFDEF EMBED}
{$IFDEF ITM02}
If Not GM02NIDataFound Then
  GM02NIDataFound := GM02NIData.LoadFromResource('GM02NI, 'DATA');
If Not GM02RoIDataFound Then
  GM02RoIDataFound := GM02RoIData.LoadFromResource('GM02RoI, 'DATA');
{$ENDIF}
If Not GM15NIDataFound Then
  GM15NIDataFound := GM15NIData.LoadFromResource('GM15NI', 'DATA');
If Not GM15RoIDataFound Then
  GM15RoIDataFound := GM15RoIData.LoadFromResource('GM15RoI', 'DATA');
{$ENDIF}

GRS80Ellipsoid.Initialize(6378137.0000, 6356752.314140);
ITMProjection.Initialize(0.99982, DegToRad(53.5), DegToRad(-8), 600000, 750000, GRS80Ellipsoid);
{$IFDEF ITM02}
// TODO: EPSG number needes to be distinct from GM15 for this to work.
ITM02CoordinateSystem.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'Irish Transverse Mercator (ITM/GM02)', 2157,
                                 ctProjected, aoXYZ, ITMBounds, vdMalinHead);
CoordinateSystems.Register(ITM02CoordinateSystem);
{$ENDIF}

ITM15CoordinateSystem.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'Irish Transverse Mercator (ITM/GM15)', 2157,
                                 ctProjected, aoXYZ, ITMBounds, vdMalinHead);
CoordinateSystems.Register(ITM15CoordinateSystem);

Finalization

{ Release any required data table structures. }
{$IFDEF ITM02}
GM02NIData.Release;
GM02RoIData.Release;
{$ENDIF}
GM15NIData.Release;
GM15RoIData.Release;

End.

