Program GITrans;

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
  Classes, SysUtils, CustApp, DOS, TransMain;

Type
  TOperatingMode = (omIO, omCGI);
Type
  TGITransApplication = Class(TCustomApplication)
  Public
    InputFileName: String;
    OutputFileName: String;
    ProtectOutputMode: Boolean;
    SilentMode: Boolean;
    OperatingMode: TOperatingMode;
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure WriteHeader;
    Procedure WriteHelp;
    Procedure WriteToConsole(Text: String);
  End;

Constructor TGITransApplication.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  CaseSensitiveOptions := False;
  StopOnException := True;
End;

Destructor TGITransApplication.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TGITransApplication.WriteHeader;
Begin
  If SilentMode Then
    Exit;
  WriteLn('GITrans - GridInQuestII spatial transformation utility.');
  WriteLn('(c) 2015 Paul F. Michell, Michell Computing.');
  WriteLn;
End;

Procedure TGITransApplication.WriteHelp;
Begin
  WriteLn('Usage: ', ChangeFileExt(ExtractFileName(ExeName),EmptyStr), ' <InputFileName> <OutputFileName> [Options]');
  WriteLn;
  WriteLn('InputFileName - File Name identifying input text file.');
  WriteLn('OutputFileName - File Name identifying output text file.');
  WriteLn;
  WriteLn('Available Options:');
  WriteLn('--help (-h):  This help information.');
  WriteLn('--list (-l):  List available coordinate reference systems.');
  WriteLn('--protect (-p):  Prevent output file from being over-written if it exists.');
  WriteLn('--silent (-s):  Supress all command line output.');
  WriteLn('--iformat (-i)<csv|tab|txt>:  Input format selection, csv by default.');
  WriteLn('--oformat (-o)<csv|tab|txt>:  Output format selection, csv by default.');
  WriteLn('--source (-s)<EPSG>:  Source coordinate system EPSG number.');
  WriteLn('--target (-t)<EPSG>:  Target coordinate system EPSG number.');
  WriteLn;
  WriteLn('CGI script web mode');
  WriteLn;
  WriteLn('Usage: ', ChangeFileExt(ExtractFileName(ExeName),EmptyStr), '?<param1>=<value1>&<param2>=<value2>&..&<paramn>=<valuen>');
  WriteLn;
  WriteLn('CGI parameters');
  WriteLn('EPSGIn|I:  Input coordinate system EPSG number.');
  WriteLn('EPSGOut|O:  Output coordinate system EPSG number.');
  WriteLn('Lat:  Input geodetic latitude value in decimal degrees.');
  WriteLn('Lon:  Input geodetic longitude value in decimal degrees.');
  WriteLn('Alt:  Optional input geodetic height value in metres.');
  WriteLn('East|E:  Input projected easting value in decimal degrees.');
  WriteLn('North|N:  Input projected northing value in decimal degrees.');
  WriteLn('Height|H:  Optional input projected height value in metres.');
  WriteLn('X:  Input cartesian X value in metres.');
  WriteLn('Y:  Input cartesian Y value in metres.');
  WriteLn('Z:  Input cartesian Z value in metres.');

End;

Procedure TGITransApplication.WriteToConsole(Text: String);
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
  With TGITransApplication.Create(Nil) Do
    Try
      { Test for CGI mode. }
      //OperatingMode := omCGI;
      If ParamCount=0 Then
        Begin
          If SameText(GetEnv('REQUEST_METHOD'),'GET') Then
            OperatingMode := omCGI
          Else If SameText(GetEnv('REQUEST_METHOD'),'POST') Then
             OperatingMode := omCGI
          Else
             OperatingMode := omIO;
        End
      Else
        OperatingMode := omIO;
      Case OperatingMode Of
      omIO:
        Begin
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
          ProcessFileTransformation(InputFileName, OutputFileName);
        End;
      omCGI:
        Begin
          ProcessCGIRequest('');
        End;
      End;
    Finally
      Free;
    End;
End.

