Program OSGMUpdate;

{ Grid InQuest II OSGM02 to OSGM15 Updater Utility Program.

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

Uses
  {$IFDEF UNIX}CWString, {$ENDIF}Classes, SysUtils, CustApp, DOS, IOMain, GeodProc;

Type
  TStringArray = Array Of String;

Type
  OSGMUpdateApplication = Class(TCustomApplication)
  Public
    FileNames: TStringArray;
    InputFileName: String;
    OutputFileName: String;
    SettingsFileName: String;
    SettingsInfo: TSettingsInfo;
    ProtectOutputMode: Boolean;
    SilentMode: Boolean;
    IsGetRequest: Boolean;
    IsPostRequest: Boolean;
    InputChar: Char;
    RequestText: String;
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function GetFileNames: TStringArray;
    Procedure WriteHeader;
    Procedure WriteHelp;
    Procedure WriteToConsole(Text: String);
  End;

Constructor OSGMUpdateApplication.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  CaseSensitiveOptions := False;
  StopOnException := True;
End;

Destructor OSGMUpdateApplication.Destroy;
Begin
  Inherited Destroy;
End;

Function OSGMUpdateApplication.GetFileNames: TStringArray;
Var
  Index: Integer;
  LastIndex: Integer;
  FileCount: Integer;
Begin
  FileCount := 0;
  LastIndex := ParamCount;
  For Index := 1 To LastIndex Do
    If Params[Index][1]<>'-' Then
      Begin
        Inc(FileCount);
        SetLength(Result, FileCount);
        Result[FileCount-1] := Params[Index];
      End;
End;

Procedure OSGMUpdateApplication.WriteHeader;
Begin
  If SilentMode Then
    Exit;
  WriteLn('OSGMUpdate - Grid InQuest II OSGM02/TN02 to OSGM15/TN15 updater utility.');
  WriteLn('(c) 2016 Paul F. Michell, Michell Computing.');
  WriteLn;
End;

Procedure OSGMUpdateApplication.WriteHelp;
Begin
  WriteLn('Usage: ', ChangeFileExt(ExtractFileName(ExeName),EmptyStr), ' <SettingsFileName> <InputFileName> <OutputFileName> [Options]');
  WriteLn;
  WriteLn('SettingsFileName - File Name defining conversion settings in XML format.');
  WriteLn('InputFileName - File Name identifying input text file.');
  WriteLn('OutputFileName - File Name identifying output text file.');
  WriteLn;
  WriteLn('Available Options:');
  WriteLn('--help (-h):  This help information.');
  WriteLn('--protect (-p):  Prevent output file from being over-written if it exists.');
  WriteLn('--silent (-s):  Supress all command line output.');
End;

Procedure OSGMUpdateApplication.WriteToConsole(Text: String);
Begin
  If Not SilentMode Then
    WriteLn(Text);
End;

Function FileNameValid(FileName: String): Boolean;
Var
  FileHandle: THandle;
Begin
  FileHandle := FileCreate(FileName);
  Result := (FileHandle<>THandle(-1));
  If Result Then
    Begin
      FileClose(FileHandle);
      DeleteFile(FileName);
    End;
End;

{$R *.res}

Begin
  With OSGMUpdateApplication.Create(Nil) Do
    Try
      SilentMode := HasOption('s', 'silent');
      WriteHeader;
      If HasOption('h', 'help') Then
        Begin
          WriteHelp;
          Exit;
        End;
      ProtectOutputMode := HasOption('p', 'protect');
      { Retrieve the IO filenames from the command-line parameters. }
      FileNames := GetFileNames;
      If Length(FileNames)>=3 Then
        Begin
          SettingsFileName := FileNames[0];
          InputFileName := FileNames[1];
          OutputFileName := FileNames[2];
        End
      Else
        Begin
          WriteToConsole('Settings, Input and Output filenames are missing or incomplete.');
          Exit;
        End;
      { Validate the settings file name. }
      SettingsFileName := ExpandFileName(SettingsFileName);
      If Not FileExists(SettingsFileName) Then
        Begin
          WriteToConsole('Settings file not found: '+SettingsFileName);
          Exit;
        End;
      { Load the setting values. }
      If Not LoadSettings(SettingsFileName, SettingsInfo, False) Then
        Begin
          WriteToConsole('Invalid settings file: '+SettingsFileName);
          Exit;
        End;
      { Set the fixed conversion settings. }
      With SettingsInfo Do
        Begin
          { By default omit the horizontal axes. }
          TargetXName := '';
          TargetYName := '';
          Case SourceSRID Of
          27700: { BNG/TN02 }
            Begin
              TargetSRID := 27700;
              { For BNG output the updated horizontal coordinates. }
              TargetXName := 'OSTN15Easting';
              TargetYName := 'OSTN15Northing';
            End;
          29903: { IG/GM02 }
            TargetSRID := 29903;
          2157: { ITM/GM02 }
            TargetSRID := 2157;
          Else
            WriteToConsole('Unknown source SRID: '+IntToStr(SourceSRID));
            Exit;
          End;
          { Output the vertical axis for all systems. }
          TargetZName := 'OSGM15Elevation';
          HorizontalDecimalPlaces := 3;
          VerticalDecimalPlaces := 3;
          ZeroFill := True;
        End;
      { Validate the input file name. }
      InputFileName := ExpandFileName(InputFileName);
      If Not FileExists(InputFileName) Then
        Begin
          WriteToConsole('Input file not found: '+InputFileName);
          Exit;
        End;
      { Validate the output file name. }
      OutputFileName := ExpandFileName(OutputFileName);
      If FileExists(OutputFileName) And ProtectOutputMode Then
        Begin
          WriteToConsole('Output file exists: '+OutputFileName);
          Exit;
        End;
      If Not FileNameValid(OutputFileName) Then
        Begin
          WriteToConsole('Output file name invalid: '+OutputFileName);
          Exit;
        End;
      { Process the file transformation. }
      ProcessFile(SettingsInfo, InputFileName, OutputFileName, Not SilentMode);
    Finally
      Free;
    End;
End.

