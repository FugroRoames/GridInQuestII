Unit TransMain;

{ Transformation Utility Main Unit.

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
  SysUtils, Math, Geometry, Geodesy, ETRS, BNG, ITM, IG;

Function BuildAvailableSystemsList(): String;
Procedure ProcessFileTransformation(Const InputFileName: String; Const OutputFileName: String);
Procedure ProcessCGIRequest(Const Query: String);

Implementation

Function BuildAvailableSystemsList: String;
Begin
  Result := CoordinateSystems.AvailableSystemsList(True);
End;

Procedure ProcessFileTransformation(Const InputFileName: String; Const OutputFileName: String);
Begin
  WriteLn('File input from: '+InputFileName);
  WriteLn('File output to: '+OutputFileName);
  WriteLn(ITMCoordinateSystem.PreferredVerticalDatum);
End;

Procedure ProcessCGIRequest(Const Query: String);
Var
  Coordinates: TCoordinates;
  GeoJSON: String;
  HasHeight: Boolean;
Begin
  HasHeight := False;
  Coordinates.X := 1.1234567890123456;
  Coordinates.Y := 2;
  Coordinates.Z := 3;
  If HasHeight Then
    GeoJSON := Format('{ "type": "Point", "coordinates": [%f, %f, %f] }',[Coordinates.X, Coordinates.Y, Coordinates.Z])
  Else
    GeoJSON := Format('{ "type": "Point", "coordinates": [%f, %f] }',[Coordinates.X, Coordinates.Y]);
  writeLn('Content-type: application/json');
  writeLn;
  writeLn(GeoJSON);
End;

(*
Uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  {$ENDIF}
  SysUtils, DOS;

Var
  InputText: String;
  InputChar: Char;
Begin
  writeLn ('Content-type: text/html');
  writeLn ('');
  WriteLn('<H1>Transform Results</H1>');

  If SameText(GetEnv('REQUEST_METHOD'),'GET') Then
    Begin
      InputText := GetEnv('QUERY_STRING');
      WriteLn(InputText);
    End;
  {
  InputText := '';
  If SameText(GetEnv('REQUEST_METHOD'),'POST') Then
    While Not EOF(Input) Do
      begin
        Read(InputChar);
        Write(InputChar);
        InputText += InputChar;
        Write('~');
      End;
  WriteLn(InputText);
  }
  WriteLn('<P>All Done</P>');
End.


*)

End.

