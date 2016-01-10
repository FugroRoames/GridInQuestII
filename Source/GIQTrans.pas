Program GIQTrans;

{ Grid InQuest II Coordinate Transformation Utility Program.

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
  Classes, SysUtils, CustApp, DOS, TransMain, GeodProc, GeoJSON;

Type
  TOperatingMode = (omIO, omCGI);

Type
  TGITransApplication = Class(TCustomApplication)
  Public
    FileNames: TStringArray;
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
  WriteLn('--silent (-i):  Supress all command line output.');
  WriteLn('--settings (-s) File Name of transformation settings XML file.');
  WriteLn;
  WriteLn('CGI command mode');
  WriteLn;
  WriteLn('Usage: ', ChangeFileExt(ExtractFileName(ExeName),EmptyStr), '?<param1>=<value1>&<param2>=<value2>&..&<paramn>=<valuen>');
  WriteLn;
  WriteLn('CGI parameters');
  WriteLn('SourceSRID:  Input coordinate system SRID number.');
  WriteLn('TargetSRID:  Output coordinate system SRID number.');
  WriteLn('PreferredDatum:  Preferred Irish vertical datum code (13 - Malin Head or 14 - Belfast).');
  WriteLn('Geometry:  Input point geometry in GeoJSON format.');
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

Function LoadFileAsText(FileName: String): String;
Var
  TextBuffer: TMemoryStream;
Begin
  TextBuffer := TMemoryStream.Create;
  TextBuffer.Create;
  TextBuffer.LoadFromFile(FileName);
  SetLength(Result, TextBuffer.Size);
  TextBuffer.Read(Result[1], TextBuffer.Size);
  TextBuffer.Free;
End;

Var
  IsGetRequest: Boolean;
  IsPostRequest: Boolean;
  InputChar: Char;
  RequestText: String;
  FileName: String;

{$R *.res}

Begin
  With TGITransApplication.Create(Nil) Do
    Try
      { Test for CGI mode. }
      If ParamCount=0 Then
        Begin
          IsGetRequest := SameText(GetEnv('REQUEST_METHOD'),'GET');
          IsPostRequest := SameText(GetEnv('REQUEST_METHOD'),'POST');
          If IsGetRequest Then
            OperatingMode := omCGI
          Else If IsPostRequest Then
             OperatingMode := omCGI
          Else
             OperatingMode := omIO;
        End
      Else
        OperatingMode := omIO;
      Case OperatingMode Of
      omIO:
        Begin
          If HasOption('CGI') Then
            Begin
              { Simulate CGI mode from file input. }
              FileName := GetOptionValue('CGI');
              FileName := ExpandFileName(FileName);
              If FileExists(FileName) Then
                Begin
                  RequestText := LoadFileAsText(FileName);
                  ProcessCGIRequest(RequestText);
                End
              Else
                Begin
                  WriteToConsole('Input file not found: '+InputFileName);
                  Exit;
                End;
              Exit;
            End;
          SilentMode := HasOption('i', 'silent');
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
          { Retrieve the IO filenames from the command-line parameters. }
          FileNames := GetNonOptions('',[]);
          If Length(FileNames)>=2 Then
            Begin
              InputFileName := FileNames[0];
              OutputFileName := FileNames[1];
            End
          Else
          Begin
            If Length(FileNames)=0 Then
              WriteToConsole('Input and output filenames not given.')
            Else
              WriteToConsole('Output filename not given.');
            Exit;
          End;
          { Validate the input file name parameter. }
          InputFileName := ExpandFileName(InputFileName);
          If Not FileExists(InputFileName) Then
            Begin
              WriteToConsole('Input file not found: '+InputFileName);
              Exit;
            End;
          { Validate the output file name parameter. }
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
          ProcessFile(InputFileName, OutputFileName, Not SilentMode);
        End;
      omCGI:
        Begin
          { Fetch the CGI request parameters. }
          If IsGetRequest Then
            RequestText := GetEnv('QUERY_STRING');
          If IsPostRequest Then
            While Not EOF(Input) Do
              Begin
                Read(InputChar);
                RequestText += InputChar;
              End;
          { Process the CGI request. }
          ProcessCGIRequest(RequestText);
        End;
      End;
    Finally
      Free;
    End;
End.

