Unit Config;

{ GridInQuest II Coordinate Transformation Utility Configuration Unit.

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
  Classes, SysUtils, DOM, XMLConf, OSTab, ITM, IG, GeodUtils;

Type
  TFormatSettings = Record
    GeodeticStyle: String;
    GeodeticUnits: TTypedOptions;
    GeodeticQuadrants: String;
    GeodeticDecimalPlaces: Integer;
    GeodeticCompactFormat: Boolean;
    GeodeticPositiveLongitude: Boolean;
    ProjectedStyle: String;
    ProjectedDecimalPlaces: Integer;
    CartesianStyle: String;
    CartesianDecimalPlaces: Integer;
    HeightStyle: String;
    HeightDecimalPlaces: Integer;
    HeightDatumSuffix: Boolean;
  End;

Var
  InteractiveSettings: TFormatSettings;
  OutputSettings: TFormatSettings;


Procedure ReadConfigOptions;
Procedure WriteConfigOptions;

Implementation

Var
  ConfigurationFolder: String;
  XMLConfig: TXMLConfig;

Const
  InteractiveSettingsKey = 'InteractiveSettings';
  OutputSettingsKey = 'OutputSettings';
  GeodeticFormatKey = 'GeodeticFormat';
  ProjectedFormatKey = 'ProjectedFormat';
  CartesianFormatKey = 'CartesianFormat';
  HeightFormatKey = 'HeightFormat';
  StyleKey = 'Style';
  UnitsKey = 'Units';
  QuadrantsKey = 'Quadrants';
  DecimalPlacesKey = 'DecimalPlaces';
  CompactWhitespaceKey = 'CompactWhitespace';
  PositiveLongitudeKey = 'PositiveLongitude';
  DatumSuffixKey = 'DatumSuffix';
  IrishSettingsKey = 'IrishSettings';
  IGPreferredDatumKey = 'IGPreferredDatum';
  ITMPreferredDatumKey = 'ITMPreferredDatum';

Procedure ReadConfigOptions;
  Function TextToUnits(Text: String): TTypedOptions;
  Begin
    Case Text Of
    'Symbols': Result := [toSexagseimalSymbols];
    'Letters': Result := [toSexagseimalLetters];
    Else
      Result := [];
    End;
  End;
Begin
  { Interactive formatting settings. }
  XMLConfig.OpenKey(InteractiveSettingsKey);
////////////
  With InteractiveSettings Do
    Begin
      XMLConfig.OpenKey(GeodeticFormatKey);
      GeodeticStyle := XMLConfig.GetValue(StyleKey, 'DMS');
      GeodeticUnits := TextToUnits(XMLConfig.GetValue(UnitsKey, EmptyStr));
      GeodeticQuadrants := XMLConfig.GetValue(QuadrantsKey, 'Signs');
      GeodeticDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 3);
      GeodeticCompactFormat := XMLConfig.GetValue(CompactWhitespaceKey, True);
      GeodeticPositiveLongitude := XMLConfig.GetValue(PositiveLongitudeKey, True);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(ProjectedFormatKey);
      ProjectedStyle := XMLConfig.GetValue(StyleKey, 'EN');
      ProjectedDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(CartesianFormatKey);
      CartesianStyle := XMLConfig.GetValue(StyleKey, 'XYZ');
      CartesianDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(HeightFormatKey);
      HeightStyle := XMLConfig.GetValue(StyleKey, 'XYZ');
      HeightDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      HeightDatumSuffix := XMLConfig.GetValue(DatumSuffixKey, True);
      XMLConfig.CloseKey;
    End;
  ////////
  XMLConfig.CloseKey;
  { Output formatting settings. }
  XMLConfig.OpenKey(OutputSettingsKey);
////////////
  With OutputSettings Do
    Begin
      XMLConfig.OpenKey(GeodeticFormatKey);
      GeodeticStyle := XMLConfig.GetValue(StyleKey, 'DMS'); //
      GeodeticUnits := TextToUnits(XMLConfig.GetValue(UnitsKey, EmptyStr));
      GeodeticQuadrants := XMLConfig.GetValue(QuadrantsKey, 'Signs');
      GeodeticDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 3);
      GeodeticCompactFormat := XMLConfig.GetValue(CompactWhitespaceKey, True);
      GeodeticPositiveLongitude := XMLConfig.GetValue(PositiveLongitudeKey, True);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(ProjectedFormatKey);
      ProjectedStyle := XMLConfig.GetValue(StyleKey, 'EN'); //
      ProjectedDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(CartesianFormatKey);
      CartesianStyle := XMLConfig.GetValue(StyleKey, 'XYZ');  //
      CartesianDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(HeightFormatKey);
      HeightStyle := XMLConfig.GetValue(StyleKey, 'XYZ');  //
      HeightDecimalPlaces := XMLConfig.GetValue(DecimalPlacesKey, 2);
      HeightDatumSuffix := XMLConfig.GetValue(DatumSuffixKey, True);
      XMLConfig.CloseKey;
    End;
////////
  XMLConfig.CloseKey;
  { Irish datum settings. }
  XMLConfig.OpenKey(IrishSettingsKey);
  With IGCoordinateSystem Do
    PreferredVerticalDatum := VerticalDataNameToCode(XMLConfig.GetValue(IGPreferredDatumKey,
                                                     VerticalDataCodeToName(PreferredVerticalDatum)));
  With ITMCoordinateSystem Do
    PreferredVerticalDatum := VerticalDataNameToCode(XMLConfig.GetValue(ITMPreferredDatumKey,
                                                     VerticalDataCodeToName(PreferredVerticalDatum)));
  XMLConfig.CloseKey;
End;

Procedure WriteConfigOptions;
  Function UnitsToText(Options: TTypedOptions): String;
  Begin
    If toSexagseimalSymbols In Options Then
      Result := 'Symbols'
    Else If toSexagseimalLetters In Options Then
      Result := 'Letters'
    Else
      Result := 'None'
  End;
Begin
  { Interactive formatting settings. }
  XMLConfig.Clear;
  XMLConfig.OpenKey(InteractiveSettingsKey);
////////////
  With InteractiveSettings Do
    Begin
      XMLConfig.OpenKey(GeodeticFormatKey);
      XMLConfig.SetValue(StyleKey, GeodeticStyle); //
      XMLConfig.SetValue(UnitsKey, UnitsToText(GeodeticUnits));
      XMLConfig.SetValue(QuadrantsKey, GeodeticQuadrants);  //
      XMLConfig.SetValue(DecimalPlacesKey, GeodeticDecimalPlaces);
      XMLConfig.SetValue(CompactWhitespaceKey, GeodeticCompactFormat);
      XMLConfig.SetValue(PositiveLongitudeKey, GeodeticPositiveLongitude);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(ProjectedFormatKey);
      XMLConfig.SetValue(StyleKey, ProjectedStyle);//
      XMLConfig.SetValue(DecimalPlacesKey, ProjectedDecimalPlaces);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(CartesianFormatKey);
      XMLConfig.SetValue(StyleKey, CartesianStyle);//
      XMLConfig.SetValue(DecimalPlacesKey, CartesianDecimalPlaces);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(HeightFormatKey);
      XMLConfig.SetValue(StyleKey, HeightStyle);//
      XMLConfig.SetValue(DecimalPlacesKey, HeightDecimalPlaces);
      XMLConfig.SetValue(DatumSuffixKey, HeightDatumSuffix);
      XMLConfig.CloseKey;
    End;
////////
  XMLConfig.CloseKey;
  { Output formatting settings. }
  XMLConfig.OpenKey(OutputSettingsKey);
  ////////////
  With OutputSettings Do
    Begin
      XMLConfig.OpenKey(GeodeticFormatKey);
      XMLConfig.SetValue(StyleKey, GeodeticStyle);  //
      XMLConfig.SetValue(UnitsKey, UnitsToText(GeodeticUnits));
      XMLConfig.SetValue(QuadrantsKey, GeodeticQuadrants);   //
      XMLConfig.SetValue(DecimalPlacesKey, GeodeticDecimalPlaces);
      XMLConfig.SetValue(CompactWhitespaceKey, GeodeticCompactFormat);
      XMLConfig.SetValue(PositiveLongitudeKey, GeodeticPositiveLongitude);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(ProjectedFormatKey);
      XMLConfig.SetValue(StyleKey, ProjectedStyle);  //
      XMLConfig.SetValue(DecimalPlacesKey, ProjectedDecimalPlaces);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(CartesianFormatKey);
      XMLConfig.SetValue(StyleKey, CartesianStyle);   //
      XMLConfig.SetValue(DecimalPlacesKey, CartesianDecimalPlaces);
      XMLConfig.CloseKey;

      XMLConfig.OpenKey(HeightFormatKey);
      XMLConfig.SetValue(StyleKey, HeightStyle);     //
      XMLConfig.SetValue(DecimalPlacesKey, HeightDecimalPlaces);
      XMLConfig.SetValue(DatumSuffixKey, HeightDatumSuffix);
      XMLConfig.CloseKey;
    End;
  ////////
  XMLConfig.CloseKey;
  { Irish datum settings. }
  XMLConfig.OpenKey(IrishSettingsKey);
  With IGCoordinateSystem Do
    XMLConfig.SetValue(IGPreferredDatumKey, VerticalDataCodeToName(PreferredVerticalDatum));
  With ITMCoordinateSystem Do
    XMLConfig.SetValue(ITMPreferredDatumKey, VerticalDataCodeToName(PreferredVerticalDatum));
  XMLConfig.CloseKey;
  XMLConfig.Flush;
End;

Function SafeForceDirectories(FolderPath: String): Boolean;
Begin
  Try
    Result := ForceDirectories(FolderPath);
  Except
    Result := False;
  End;
End;

Initialization

ConfigurationFolder := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
SafeForceDirectories(ConfigurationFolder);
XMLConfig := TXMLConfig.Create(Nil);
XMLConfig.RootName := 'Configuration';
XMLConfig.Filename := ConfigurationFolder+ApplicationName+ConfigExtension;

Finalization

XMLConfig.Free;

End.

