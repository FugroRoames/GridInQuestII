Unit CGIMain;

{ CGI Processor Main Unit.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
  for more details. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, Geodesy, GeodProc,
  ETRS, BNG, ITM, IG, OSTab, GeoJSON;

Procedure ProcessCGIRequest(Const RequestText: String);

Implementation

Procedure ProcessCGIRequest(Const RequestText: String);
Var
  Parameters: TStringList;
  SourceSRID: Integer;
  TargetSRID: Integer;
  InputCoordinates: TCoordinates;
  OutputCoordinates: TCoordinates;
  AttributeName: String;
  DatumCode: TVerticalDatumCode;
  DatumCodeText: String;
  GeometryJSONText: String;
  HasHeight: Boolean;
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
      DatumCode := TVerticalDatumCode(StrToIntDef(Values['PreferredDatum'], 0));
      GeometryJSONText := Values['Geometry'];
    Finally
      Free;
    End;
  If JSONGeometryType(GeometryJSONText)=jtPoint Then
    Begin
      HasHeight := JSONPointIncludesZValue(GeometryJSONText);
      InputCoordinates := JSONPointToCoordinates(GeometryJSONText);
      If IsSRIDGeodeticSystem(SourceSRID) Then
        InputCoordinates := GeodeticDegToRad(InputCoordinates);
      TransformCoordinates(SourceSRID, TargetSRID, InputCoordinates, OutputCoordinates, DatumCode);
      If IsSRIDGeodeticSystem(TargetSRID) Then
        OutputCoordinates := GeodeticDegToRad(OutputCoordinates);
      DatumCodeText := IntToStr(Integer(DatumCode));
      AttributeName := 'datum';
      GeometryJSONText := CoordinatesAndAttributeToJSONPoint(OutputCoordinates, AttributeName,
                                                             DatumCodeText, HasHeight);
      WriteLn('Content-type: application/json');
      WriteLn;
      WriteLn(GeometryJSONText);
    End
  Else
    Begin
      WriteLn('Status: 400 Un-handeled or missing GeoJSON geometry type.');
      WriteLn;
    End;
End;

End.

