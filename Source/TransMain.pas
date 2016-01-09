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
  Classes, SysUtils, fpJSON, JSONParser, Math, Geometry, Geodesy, ETRS, BNG, ITM, IG;

Function BuildAvailableSystemsList(): String;
Function ConvertCoordinates(SourceEPSG, TargetEPSG: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: Integer): Boolean;
Procedure ProcessFileTransformation(Const InputFileName: String; Const OutputFileName: String);
Procedure ProcessCGIRequest(Const RequestText: String);

Implementation

Function BuildAvailableSystemsList: String;
Begin
  Result := CoordinateSystems.AvailableSystemsList(True);
End;

Function ConvertCoordinates(SourceEPSG, TargetEPSG: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: Integer): Boolean;
Var
  SourcePointer: TCoordinateSystemPointer;
  TargetPointer: TCoordinateSystemPointer;
  GeocentricCoordinates: TCoordinates;
Begin
  Result := False;
  With CoordinateSystems Do
    SourcePointer := Pointers(FindEPSGNumber(SourceEPSG));
  With CoordinateSystems Do
    TargetPointer := Pointers(FindEPSGNumber(TargetEPSG));
  If Assigned(SourcePointer) Then
    GeocentricCoordinates := SourcePointer^.ConvertToGeocentric(InputCoordinates)
  Else
    Exit;
  If Assigned(TargetPointer) Then
    Begin
      TargetPointer^.PreferredVerticalDatum := TVerticalDatumCode(DatumCode);
      OutputCoordinates := TargetPointer^.ConvertFromGeocentric(GeocentricCoordinates);
      DatumCode := Integer(TargetPointer^.LastVerticalDatum);
    End
  Else
    Exit;
  Result := True;
End;

Procedure ProcessFileTransformation(Const InputFileName: String; Const OutputFileName: String);
Begin
  WriteLn('File input from: '+InputFileName);
  WriteLn('File output to: '+OutputFileName);
  WriteLn(ITM15CoordinateSystem.PreferredVerticalDatum);
End;

Procedure ProcessCGIRequest(Const RequestText: String);
Var
  Parameters: TStringList;
  SourceSRID: Integer;
  TargetSRID: Integer;
  InputJSON: TJSONData;
  InputCoordinates: TCoordinates;
  OutputCoordinates: TCoordinates;
  DatumCode: Integer;
  GeometryJSONText: String;
  HasHeight: Boolean;
  Function ExtractGeometryJSONCoordinate(Index: Integer): TCoordinate;
  Var
    ValueJSON: TJSONData;
  Begin
    ValueJSON := InputJSON.FindPath('coordinates['+IntToStr(Index)+']');
    If ValueJSON=Nil Then
      Result := 0
    Else
      Result := ValueJSON.AsFloat;
    If Index=2 Then
      HasHeight := (ValueJSON<>Nil);
  End;
Begin
  HasHeight := False;
  Parameters := TStringList.Create;
  With Parameters Do
    Try
      Delimiter := '&';
      StrictDelimiter := True;
      DelimitedText := RequestText;
      SourceSRID := StrToIntDef(Values['SourceSRID'], 0);
      TargetSRID := StrToIntDef(Values['TargetSRID'], 0);
      DatumCode := StrToIntDef(Values['PreferredDatum'], 0);
      GeometryJSONText := Values['Geometry'];
    Finally
      Free;
    End;
  InputJSON := GetJSON(GeometryJSONText);
  InputCoordinates.X := ExtractGeometryJSONCoordinate(0);
  InputCoordinates.Y := ExtractGeometryJSONCoordinate(1);
  InputCoordinates.Z := ExtractGeometryJSONCoordinate(2);
  InputCoordinates := GeodeticDegToRad(InputCoordinates);
  ConvertCoordinates(SourceSRID, TargetSRID, InputCoordinates, OutputCoordinates, DatumCode);
  With OutputCoordinates Do
    If HasHeight Then
      GeometryJSONText := Format('{"type":"Point","coordinates":[%G,%G,%G],"datum":%D}',[X, Y, Z, DatumCode])
    Else
      GeometryJSONText := Format('{"type":"Point","coordinates":[%G,%G],"datum":%D}',[X, Y, DatumCode]);

  writeLn('Content-type: application/json');
  writeLn;
  writeLn(GeometryJSONText);
End;

End.

