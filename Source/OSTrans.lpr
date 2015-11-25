Program OSTrans;

{ OS/OSI/LPS Coordinate Transformation Utility Program.

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
  Classes, SysUtils, CustApp, TransMain;

Type
  TOSTransApplication = Class(TCustomApplication)
  Public
    InputFileName: String;
    OutputFileName: String;
    ProtectOutputMode: Boolean;
    SilentMode: Boolean;
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure WriteHeader;
    Procedure WriteHelp;
    Procedure WriteToConsole(Text: String);
  End;

Constructor TOSTransApplication.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  CaseSensitiveOptions := False;
  StopOnException := True;
End;

Destructor TOSTransApplication.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TOSTransApplication.WriteHeader;
Begin
  If SilentMode Then
    Exit;
  WriteLn('GITrans - GridInQuestII spatial transformation utility.');
  WriteLn('(c) 2015 Paul F. Michell, Michell Computing.');
  WriteLn;
End;

Procedure TOSTransApplication.WriteHelp;
Begin
  WriteLn('Usage: ', ChangeFileExt(ExtractFileName(ExeName),EmptyStr), ' <InputFileName> <OutputFileName> [Options]');
  WriteLn;
  WriteLn('InputFileName - File Name identifying input text file.');
  WriteLn('OutputFileName - File Name identifying output text file.');
  WriteLn;
  WriteLn('Available Options:');
  WriteLn('--help (-h):  This help information.');
  WriteLn('--list (-l):  List available coordinate reference systems.');
  WriteLn('--protect (-p): Prevent output file from being over-written if it exists.');
  WriteLn('--silent (-s): Supress all command line output.');
  WriteLn('--iformat (-i)<csv|tab|txt>: Input format selection, csv by default.');
  WriteLn('--oformat (-o)<csv|tab|txt>: Output format selection, csv by default.');
  WriteLn('--source (-s)<EPSG>: Source coordinate system EPSG number.');
  WriteLn('--target (-t)<EPSG>: Target coordinate system EPSG number.');
  WriteLn;
End;

Procedure TOSTransApplication.WriteToConsole(Text: String);
Begin
  If Not SilentMode Then
    WriteLn(Text);
End;

Function FileNameValid(FileName: String): Boolean;
Var
  FileHandle: THandle;
Begin
  FileHandle := FileCreate(FileName);
  Result := (FileHandle<>-1);
  If Result Then
    Begin
      FileClose(FileHandle);
      DeleteFile(FileName);
    End;
End;

//{$R *.rc}

Begin
  With TOSTransApplication.Create(Nil) Do
    Try
      SilentMode := HasOption('s', 'silent');
      WriteHeader;
      If HasOption('h', 'help') Then
        Begin
          WriteHelp;
          Exit;
        End;
      If HasOption('l', 'list') Then
        Begin
          WriteToConsole('Available Coordinate Systems:');
          WriteToConsole(BuildAvailableSystemsList);
          Exit;
        End;
      ProtectOutputMode := HasOption('p', 'protect');
      { Read the input file name parameter. }
      InputFileName := ParamStr(1);
      InputFileName := ExpandFileName(InputFileName);
      If Not FileExists(InputFileName) Then
        Begin
          WriteToConsole('Input file not found: '+InputFileName);
          Exit;
        End;
      { Read the output file name parameter. }
      OutputFileName := ParamStr(2);
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
{
        Begin
          PackageCode := StrToIntDef(GetOptionValue('k', 'package'), 0);
        End
      Else
        PackageCode := 0;
      If HasOption('i', 'iformat') Then
        Begin
          FormatName := GetOptionValue('f', 'format');
          If SameText(FormatName, 'TXT') Then
            OutputFormat := ofTXT
          Else If SameText(FormatName, 'TAB') Then
            OutputFormat := ofTAB
          Else If SameText(FormatName, 'CSV') Then
            OutputFormat := ofCSV
          Else
            OutputFormat := ofCSV;
        End
      Else
        OutputFormat := ofCSV;}
      ProcessTransformation(InputFileName, OutputFileName);
    Finally
      Free;
    End;
End.

