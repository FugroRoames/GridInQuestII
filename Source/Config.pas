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
  Classes, SysUtils, DOM, XMLConf, OSTab, ITM, IG;

Procedure ReadConfigOptions;
Procedure WriteConfigOptions;

Implementation

Var
  ConfigurationFolder: String;
  XMLConfig: TXMLConfig;

Const
  IGPreferredDatumKey = 'IGPreferredDatum';
  ITMPreferredDatumKey = 'ITMPreferredDatum';

Procedure ReadConfigOptions;
Begin
  With IGCoordinateSystem Do
    PreferredVerticalDatum := VerticalDataNameToCode(XMLConfig.GetValue(IGPreferredDatumKey,
                                                     VerticalDataCodeToName(PreferredVerticalDatum)));
  With ITMCoordinateSystem Do
    PreferredVerticalDatum := VerticalDataNameToCode(XMLConfig.GetValue(ITMPreferredDatumKey,
                                                     VerticalDataCodeToName(PreferredVerticalDatum)));
End;

Procedure WriteConfigOptions;
Begin
  With IGCoordinateSystem Do
    XMLConfig.SetValue(IGPreferredDatumKey, VerticalDataCodeToName(PreferredVerticalDatum));
  With ITMCoordinateSystem Do
    XMLConfig.SetValue(ITMPreferredDatumKey, VerticalDataCodeToName(PreferredVerticalDatum));
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

