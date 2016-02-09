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
//{$DEFINE EMBED}

{ Define to include ITM using GM02 as an additional coordinate system. }
//{$DEFINE ITM02}

Type
  TITMCoordinateSystem = Object(TCoordinateSystem)
//    PreferredVerticalDatum: TVerticalDatumCode; { These fields should be define here but have been moved to the parent object in the Geodesy unit. }
//    LastVerticalDatum: TVerticalDatumCode; { They have been moved to avoid a compiler bug that incorrectly handles object inheritance. }
    VerticalModel: TOSVerticalModel;
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewSRIDNumber: Integer; NewRevision: Integer;
                           NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                           NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode; NewVerticalModel: TOSVerticalModel);
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
{$IFDEF ITM02}
  ITM02CoordinateSystem: TITMCoordinateSystem;
{$ENDIF}
  ITM15CoordinateSystem: TITMCoordinateSystem;

Const
  ITMBounds: TGeodeticBounds = (Western: -10.98*PI/180; Southern: 51.24*PI/180; Eastern: -5.27*PI/180; Northern: 55.63*PI/180);

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

Constructor TITMCoordinateSystem.Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewSRIDNumber: Integer;
                                            NewRevision: Integer; NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder;
                                            NewBounds: TGeodeticBounds; NewPreferredVerticalDatum: TVerticalDatumCode; NewVerticalModel: TOSVerticalModel);
Begin
  Inherited Initialize(NewName, NewAbbreviation, NewDescription, NewSRIDNumber, NewRevision,
                             NewCoordinateType, NewAxisOrder, NewBounds);
  PreferredVerticalDatum := NewPreferredVerticalDatum;
  LastVerticalDatum := vdNone;
  VerticalModel := NewVerticalModel;
End;

Function TITMCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Var
  GeodeticCoordinates: TCoordinates;
Begin
  If ITMCoordinatesToWGS84Coordinates(Coordinates, VerticalModel, PreferredVerticalDatum, GeodeticCoordinates, LastVerticalDatum) Then
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
    If WGS84CoordinatesToITMCoordinates(GeodeticCoordinates, VerticalModel, PreferredVerticalDatum, Result, LastVerticalDatum) Then
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
  NIValid := BilinearGridInterpolationParameters(NIGMData.Header, Coordinates, GridScale, NIParameters);
  If NIValid Then
    NIValid := ParametersValid(NIParameters, NIGMData.Header);
  RoIValid := BilinearGridInterpolationParameters(RoIGMData.Header, Coordinates, GridScale, RoIParameters);
  If RoIValid Then
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
  If RoIValid Then
    If PreferredDatum=vdMalinHead Then
      Begin
        Result := SetZoneHeight(dzRoI, OutputCoordinates, OutputDatum, AdjustDirection);
        Exit;
      End;
  If NIValid Then
    If PreferredDatum=vdBelfast Then
      Begin
        Result := SetZoneHeight(dzNI, OutputCoordinates, OutputDatum, AdjustDirection);
        Exit;
      End;
  OutputDatum := vdNone;
  Result := False;
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

{$IFDEF WINDOWS}
ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance)));
{$ELSE}
ProgramFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$ENDIF}

{ Construct data table filenames. }
{$IFDEF ITM02}
GM02NIFileName := ProgramFolder+'GM02NI.dat';
GM02RoIFileName := ProgramFolder+'GM02RoI.dat';
{$ENDIF}
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
ITM02CoordinateSystem.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'Irish Transverse Mercator (ITM/GM02)', 2157, 2002,
                                 ctProjected, aoXYZ, ITMBounds, vdMalinHead, vmGM02);
CoordinateSystems.Register(ITM02CoordinateSystem);
{$ENDIF}

ITM15CoordinateSystem.Initialize('Irish Transverse Mercator', 'IRENET95',
                                 'Irish Transverse Mercator (ITM/GM15)', 2157, 2015,
                                 ctProjected, aoXYZ, ITMBounds, vdMalinHead, vmGM15);
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

