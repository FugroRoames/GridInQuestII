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
  Classes, SysUtils, Math, Geometry, Geodesy, GeodProc, ETRS, BNG, ITM, IG, GeoJSON, DataStreams;

Function BuildAvailableSystemsList(): String;
Procedure ProcessFile(Const InputFileName: String; Const OutputFileName: String; ShowProgress: Boolean);
Procedure ProcessCGIRequest(Const RequestText: String);

Implementation

Function BuildAvailableSystemsList: String;
Begin
  Result := CoordinateSystems.AvailableSystemsList(True);
End;

Type
  TProgressHandler = Class
  Private
    LastProgress: Integer;
  Public
    Procedure DoLoadProgress(Sender: TObject; Progress: Integer);
    Procedure DoParseProgress(Sender: TObject; Progress: Integer);
//    Procedure DoTransformProgress(Sender: TObject; Progress: Integer);
//    Procedure DoSaveProgress(Sender: TObject; Progress: Integer);
  End;

Procedure TProgressHandler.DoLoadProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      Write('Loading Data.');
      LastProgress := Progress;
    End
  Else If Progress=100 Then
    Begin
      WriteLn();
      WriteLn('Loading Finished');
      LastProgress := Progress;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write('.');
        LastProgress := Progress;
    End;
End;

Procedure TProgressHandler.DoParseProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      Write('Parsing Data.');
      LastProgress := Progress;
    End
  Else If Progress=100 Then
    Begin
      WriteLn();
      WriteLn('Parsing Finished');
      LastProgress := Progress;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write('.');
        LastProgress := Progress;
    End;
End;

Procedure ProcessFile(Const InputFileName: String; Const OutputFileName: String; ShowProgress: Boolean);
Var
  ProgressHandler: TProgressHandler;
  InputData: TDataStream;
Begin
  InputData := TDataStream.Create;
  If ShowProgress Then
    Begin
      ProgressHandler := TProgressHandler.Create;
      InputData.OnLoadProgress := @ProgressHandler.DoLoadProgress;
      InputData.OnParseProgress := @ProgressHandler.DoParseProgress;
    End;
  Try
    Try
      InputData.LoadFromFile(InputFileName);
      If InputData.RecordCount<=0 Then
        Begin
          WriteLn('No readable data found in file: '+InputFileName);
          Exit;
        End;

      WriteLn('File output to: '+OutputFileName);
    Except
      On E:Exception Do
        Begin
          WriteLn('Insufficient memory to load: '+InputFileName);
          Exit;
        End;
    End;
  Finally
    If ShowProgress Then
      ProgressHandler.Free;
    FreeAndNil(InputData);
    //InputFirstFieldIndex := -1;
    //InputSecondFieldIndex := -1;
    //SetLength(OutputCoordinates, 0);
    //SetLength(OutputData, 0);
  End;
End;

Procedure ProcessCGIRequest(Const RequestText: String);
Var
  Parameters: TStringList;
  SourceSRID: Integer;
  TargetSRID: Integer;
  InputCoordinates: TCoordinates;
  OutputCoordinates: TCoordinates;
  AttributeName: String;
  DatumCode: Integer;
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
      DatumCode := StrToIntDef(Values['PreferredDatum'], 0);
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
      DatumCodeText := IntToStr(DatumCode);
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

