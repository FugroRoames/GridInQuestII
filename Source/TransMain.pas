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
  Classes, SysUtils, DOM, XMLConf, Math, Geometry, Geodesy, GeodProc,
  ETRS, BNG, ITM, IG, OSTab, GeoJSON, DataStreams;

Function BuildAvailableSystemsList(): String;
Procedure ProcessFile(Const SettingsFileName: String; Const InputFileName: String;
                      Const OutputFileName: String; ShowProgress: Boolean);
Procedure ProcessCGIRequest(Const RequestText: String);

Implementation

Const
  InputKey = 'InputSettings';
  OutputKey = 'OutputSettings';
  SRIDNumberKey = 'SRIDNumber';
  FormatKey = 'Format';
  DelimitedKey = 'Delimited';
  FixedKey = 'Fixed';
  FieldTerminatorKey = 'FieldTerminator';
  FieldBreaksKey = 'FieldBreaks';
  ConsecutiveDelimitersKey = 'ConsecutiveDelimiters';
  TextDelimiterKey = 'TextDelimiter';
  NameRowKey = 'NameRow';
  FirstRowKey = 'FirstRow';
  LastRowKey = 'LastRow';
  ValueKey = 'Value';
  XColumnKey = 'XColumn';
  YColumnKey = 'YColumn';
  ZColumnKey = 'ZColumn';
  IncludeDatumKey = 'IncludeDatum';
  PreferredDatumKey = 'PreferredDatum';

Function BuildAvailableSystemsList: String;
Begin
  Result := CoordinateSystems.AvailableSystemsList(True);
End;

Type
  TProgressHandler = Class
  Private
    LastProgress: Integer;
  Public
    Active: Boolean;
    SaveCount: Integer;
    TransformCount: Integer;
    Procedure DoLoadProgress(Sender: TObject; Progress: Integer);
    Procedure DoParseProgress(Sender: TObject; Progress: Integer);
    Procedure DoTransformProgress(Sender: TObject; Progress: Integer);
    Procedure DoSaveProgress(Sender: TObject; Progress: Integer);
    Procedure Message(Text: String);
    Procedure Start;
    Procedure StartTransform(Count: Integer);
    Procedure StepTransform(Index: Integer);
    Procedure StopTransform;
    Procedure StartSave(Count: Integer);
    Procedure StepSave(Index: Integer);
    Procedure StopSave;
  End;

Procedure TProgressHandler.DoLoadProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      If LastProgress<>0 Then
        Begin
          Write('Loading Data.');
          LastProgress := Progress;
        End;
    End
  Else If Progress=100 Then
    Begin
      If LastProgress<>100 Then
        Begin
          WriteLn();
          WriteLn('Loading Finished');
          LastProgress := Progress;
        End;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write(StringOfChar('.',(Progress-LastProgress) Div 5));
        LastProgress := Progress;
      End;
End;

Procedure TProgressHandler.DoParseProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      If LastProgress<>0 Then
        Begin
          Write('Parsing Data.');
          LastProgress := Progress;
        End;
    End
  Else If Progress=100 Then
    Begin
      If LastProgress<>100 Then
        Begin
          WriteLn();
          WriteLn('Parsing Finished');
          LastProgress := Progress;
        End;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write(StringOfChar('.',(Progress-LastProgress) Div 5));
        LastProgress := Progress;
      End;
End;

Procedure TProgressHandler.DoTransformProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      If LastProgress<>0 Then
        Begin
          Write('Transforming Data.');
          LastProgress := Progress;
        End;
    End
  Else If Progress=100 Then
    Begin
      If LastProgress<>100 Then
        Begin
          WriteLn();
          WriteLn('Transforming Finished');
          LastProgress := Progress;
        End;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write('.');
        LastProgress := Progress;
      End;
End;

Procedure TProgressHandler.DoSaveProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    Begin
      If LastProgress<>0 Then
        Begin
          Write('Saveing Data.');
          LastProgress := Progress;
        End;
    End
  Else If Progress=100 Then
    Begin
      If LastProgress<>100 Then
        Begin
          WriteLn();
          WriteLn('Saveing Finished');
          LastProgress := Progress;
        End;
    End
  Else
    If Progress>=LastProgress+5 Then
      Begin
        Write('.');
        LastProgress := Progress;
      End;
End;

Procedure TProgressHandler.Message(Text: String);
Begin
  If Active Then
    WriteLn(Text);
End;

Procedure TProgressHandler.Start;
Begin
  LastProgress := -1;
End;

Procedure TProgressHandler.StartTransform(Count: Integer);
Begin
  TransformCount := Count;
  LastProgress := -1;
  If Active Then
    DoTransformProgress(Self, 0);
End;

Procedure TProgressHandler.StepTransform(Index: Integer);
Var
  Progress: Integer;
Begin
  Progress := Integer(Int64(100*Int64(Index)) Div TransformCount);
  If Active Then
    DoTransformProgress(Self, Progress);
End;

Procedure TProgressHandler.StopTransform;
Begin
  If Active Then
    DoTransformProgress(Self, 100);
End;

Procedure TProgressHandler.StartSave(Count: Integer);
Begin
  SaveCount := Count;
  LastProgress := -1;
  If Active Then
    DoSaveProgress(Self, 0);
End;

Procedure TProgressHandler.StepSave(Index: Integer);
Var
  Progress: Integer;
Begin
  Progress := Integer(Int64(100*Int64(Index)) Div SaveCount);
  If Active Then
    DoSaveProgress(Self, Progress);
End;

Procedure TProgressHandler.StopSave;
Begin
  If Active Then
    DoSaveProgress(Self, 100);
End;

Type
  TInputInfo = Record
    FormatType: TFormatType;
    ConsecutiveDelimiters: Boolean;
    BreaksText: String;
    SourceSRID: Integer;
    TargetSRID: Integer;
    PreferredDatumCode: TVerticalDatumCode;
    FirstFieldIndex: Integer;
    SecondFieldIndex: Integer;
    ThirdFieldIndex: Integer;
    FieldTerminator: Char;
    TextDelimiter: Char;
    IncludeDatum: Boolean;
    IncludeNames: Boolean;
    NameRow: Integer;
    FirstRow: Integer;
    LastRow: Integer;
  End;

Type
  TOutputItem = Record
    Coordinates: TCoordinates;
    DatumCode: TVerticalDatumCode;
  End;
  TOutputData = Array Of TOutputItem;

Function LoadSettings(LoadFileName: String; Var Settings: TInputInfo): Boolean;
Var
  XMLSettings: TXMLConfig;
  XColumnIndex: Integer;
  YColumnIndex: Integer;
  ZColumnIndex: Integer;
Begin
  Result := False;
  XMLSettings := TXMLConfig.Create(Nil);
  With XMLSettings, Settings Do
    Begin
      { Open settings configuration file. }
      RootName := 'Settings';
      Filename := LoadFileName;
      { Read input settings. }
      OpenKey(InputKey);
        OpenKey(FormatKey);
          Case GetValue(ValueKey, DelimitedKey) Of
          DelimitedKey: FormatType := ftDelimited;
          FixedKey: FormatType := ftFixed;
          End;
        CloseKey;
        OpenKey(FieldTerminatorKey);
          FieldTerminator := String(GetValue(ValueKey, ','))[1];
        CloseKey;
        OpenKey(ConsecutiveDelimitersKey);
          ConsecutiveDelimiters := GetValue(ValueKey, False);
        CloseKey;
        OpenKey(FieldBreaksKey);
          BreaksText := GetValue(ValueKey, '');
        CloseKey;
        OpenKey(TextDelimiterKey);
          TextDelimiter := String(GetValue(ValueKey, #0))[1];
        CloseKey;
        OpenKey(NameRowKey);
          NameRow := GetValue(ValueKey, 0)-1;
        CloseKey;
        OpenKey(FirstRowKey);
          FirstRow := GetValue(ValueKey, 1)-1;
        CloseKey;
        OpenKey(LastRowKey);
          LastRow := GetValue(ValueKey, 0)-1;
        CloseKey;
        OpenKey(SRIDNumberKey);
          SourceSRID := GetValue(ValueKey, 0);
          If SourceSRID=0 Then
            Exit;
        CloseKey;
        OpenKey(XColumnKey);
          XColumnIndex := GetValue(ValueKey, 0)-1;
        CloseKey;
        OpenKey(YColumnKey);
          YColumnIndex := GetValue(ValueKey, 0)-1;
        CloseKey;
        OpenKey(ZColumnKey);
          ZColumnIndex := GetValue(ValueKey, 0)-1;
        CloseKey;
      CloseKey;
      { Assign the required column indicies. }
      Case SRIDToSystemPointer(SourceSRID)^.AxisOrder Of
      aoXYZ:
        Begin
          FirstFieldIndex := XColumnIndex;
          SecondFieldIndex := YColumnIndex;
        End;
      aoYXZ:
        Begin
          FirstFieldIndex := YColumnIndex;
          SecondFieldIndex := XColumnIndex;
        End;
      End;
      If (FirstFieldIndex=-1) Or (SecondFieldIndex=-1) Then
        Exit;
      ThirdFieldIndex := ZColumnIndex;
      { Read output settings. }
      OpenKey(OutputKey);
        OpenKey(SRIDNumberKey);
          TargetSRID := GetValue(ValueKey, 0);
          If TargetSRID=0 Then
            Exit;
        CloseKey;
        OpenKey(NameRowKey);
          IncludeNames := (GetValue(ValueKey, 0)<>0);
        CloseKey;
        OpenKey(PreferredDatumKey);
          PreferredDatumCode := TVerticalDatumCode(GetValue(ValueKey, 0));
        CloseKey;
        OpenKey(IncludeDatumKey);
          IncludeDatum := GetValue(ValueKey, False);
        CloseKey;
      CloseKey;
      { Close settings configuration file. }
      Free;
    End;
  Result := True;
End;

Procedure ProcessFile(Const SettingsFileName: String; Const InputFileName: String;
                      Const OutputFileName: String; ShowProgress: Boolean);
Var
  Progress: TProgressHandler;
  InputData: TDataStream;
  InputInfo: TInputInfo;
  RecordIndex: Integer;
  LastRecordIndex: Integer;
  InputCoordinates: TCoordinates;
  GeocentricCoordinates: TCoordinates;
  OutputData: TOutputData;
  OutputFile: TFileStream;
  OutputText: String;
  SRIDText: String;
  SourceSystemPointer: TCoordinateSystemPointer;
  TargetSystemPointer: TCoordinateSystemPointer;
  IncludeZAxis: Boolean;
  SourceIsGeodetic: Boolean;
  TargetIsGeodetic: Boolean;
  Procedure ParseBreaksList(BreaksText: String);
  Var
    BreaksList: TStringList;
    Index, LastIndex: Integer;
  Begin
    If InputData.FormatType=ftFixed Then
      Begin
        BreaksList := TStringList.Create;
        BreaksList.CommaText := BreaksText;
        InputData.FieldCount := BreaksList.Count;
        LastIndex := InputData.FieldCount-1;
        For Index := 0 To LastIndex Do
          Begin
            InputData.FieldStarts[Index] := StrToInt(BreaksList[Index]);
            If Index>0 Then
              InputData.FieldLengths[Index-1] := 1+InputData.FieldStarts[Index]-InputData.FieldStarts[Index-1];
          End;
        InputData.FieldLengths[LastIndex] := 1+InputData.RowLength(InputData.FirstRow)-InputData.FieldStarts[LastIndex];
        BreaksList.Free;
      End;
  End;
  Function InputDataToCoordinates(Index: Integer): TCoordinates;
  Begin
    With InputData, InputInfo Do
      Begin
        { Locate the required record. }
        RecordNumber := Index;
        { Construct the input coordinate. }
        Case SourceSystemPointer^.AxisOrder Of
        aoXYZ:
          Begin
            Result.X := StrToFloatDef(Fields[FirstFieldIndex], 0);
            Result.Y:= StrToFloatDef(Fields[SecondFieldIndex], 0);
          End;
        aoYXZ:
          Begin
            Result.Y := StrToFloatDef(Fields[FirstFieldIndex], 0);
            Result.X := StrToFloatDef(Fields[SecondFieldIndex], 0);
          End;
        End;
        { Add the third coordinate name if needed. }
        If ThirdFieldIndex<>-1 Then
          Result.Z := StrToFloatDef(Fields[ThirdFieldIndex], 0)
        Else
          Result.Z := 0;
      End;
  End;
  Function AxisShortName(AxisIndex: Integer): String;
  Begin
    With TargetSystemPointer^ Do
      Case AxisTypeFromIndex(AxisIndex, AxisOrder) Of
      atXAxis: Result := AxisNames.ShortX;
      atYAxis: Result := AxisNames.ShortY;
      atZAxis: Result := AxisNames.ShortZ;
      End;
  End;
  Function AxisCoordinateValue(AxisIndex: Integer; Coordinates: TCoordinates): String;
  Begin
    With TargetSystemPointer^ Do
      Case AxisTypeFromIndex(AxisIndex, AxisOrder) Of
      atXAxis: Result := FloatToStr(Coordinates.X);
      atYAxis: Result := FloatToStr(Coordinates.Y);
      atZAxis: Result := FloatToStr(Coordinates.Z);
      End;
  End;
  Procedure AddFieldValue(Text: String);
  Begin
    With InputInfo Do
      Begin
        OutputText := OutputText+FieldTerminator;
        If TextDelimiter=#0 Then
          OutputText := OutputText+Text
        Else
          OutputText := OutputText+TextDelimiter+
                        StringReplace(Text, TextDelimiter,TextDelimiter+TextDelimiter,
                                      [rfReplaceAll])+TextDelimiter;
      End;
  End;
Begin
  IncludeZAxis := False;
  InputData := TDataStream.Create;
  Try
    InputData.MinProgressSize := 0;
    Progress := TProgressHandler.Create;
    Progress.Active := ShowProgress;
    { Fetch the setting values. }
    If Not LoadSettings(SettingsFileName, InputInfo) Then
      Begin
        Progress.Message('ERROR: Invalid settings file: '+SettingsFileName);
        Exit;
      End;
    If ShowProgress Then
      Begin
        InputData.OnLoadProgress := @Progress.DoLoadProgress;
        InputData.OnParseProgress := @Progress.DoParseProgress;
      End;
    Try
      Progress.Message('Opening file: '+InputFileName);
      Progress.Start;
      { Initialise the input data settings. }
      InputData.LoadFromFile(InputFileName);
      InputData.FormatType := InputInfo.FormatType;
      If InputData.FormatType=ftFixed Then
        ParseBreaksList(InputInfo.BreaksText);
      InputData.FirstRow := InputInfo.FirstRow;
      InputData.LastRow := InputInfo.LastRow;
      InputData.NameRow := InputInfo.NameRow;
      InputData.FieldTerminator := InputInfo.FieldTerminator;
      InputData.TextDelimiter := InputInfo.TextDelimiter;
      If InputData.RecordCount<=0 Then
        Begin
          Progress.Message('No readable data found in file: '+InputFileName);
          Exit;
        End;
    Except
      On E:Exception Do
        Begin
          Progress.Message('ERROR: Insufficient memory to load: '+InputFileName);
          Exit;
        End;
    End;
    { Fetch coordinate system information. }
    SourceSystemPointer := SRIDToSystemPointer(InputInfo.SourceSRID);
    TargetSystemPointer := SRIDToSystemPointer(InputInfo.TargetSRID);
    SourceIsGeodetic := IsSRIDGeodeticSystem(InputInfo.SourceSRID);
    TargetIsGeodetic := IsSRIDGeodeticSystem(InputInfo.TargetSRID);
    TargetSystemPointer^.PreferredVerticalDatum := InputInfo.PreferredDatumCode;
    { Transform data records. }
    Progress.StartTransform(InputData.RecordCount);
    SetLength(OutputData, InputData.RecordCount);
    LastRecordIndex := InputData.RecordCount-1;
    For RecordIndex := 0 To LastRecordIndex Do
      Begin
        { Construct Input coordinates. }
        InputCoordinates := InputDataToCoordinates(RecordIndex);
        If SourceIsGeodetic Then
          InputCoordinates := GeodeticDegToRad(InputCoordinates);
        { Transform to geocentric. }
        GeocentricCoordinates := SourceSystemPointer^.ConvertToGeocentric(InputCoordinates);
        { Calculate the output coordinates}
        With OutputData[RecordIndex] Do
          Begin
            Coordinates := TargetSystemPointer^.ConvertFromGeocentric(GeocentricCoordinates);
            If TargetIsGeodetic Then
              Coordinates := GeodeticDegToRad(Coordinates);
            DatumCode := TargetSystemPointer^.LastVerticalDatum;
          End;
        { Update progress display. }
        Progress.StepTransform(RecordIndex);
      End;
    Progress.StopTransform;
    Progress.Message('Writing output to: '+OutputFileName);
    With InputInfo, TargetSystemPointer^ Do
      Try
        IncludeZAxis := ((ThirdFieldIndex<>-1) Or (CoordinateType=ctCartesian));
        Progress.StartSave(InputData.RecordCount);
        OutputFile := TFileStream.Create(OutputFileName, fmCreate);
        { Write the header line for the output file. }
        If IncludeNames Then
          Begin
            OutputText := InputData.NamesAsText(FieldTerminator, TextDelimiter);
            SRIDText := IntToStr(SRIDNumber);
            AddFieldValue(SRIDText+'-'+AxisShortName(0));
            AddFieldValue(SRIDText+'-'+AxisShortName(1));
            { Output the third coordinate name if needed. }
            If IncludeZAxis Then
              AddFieldValue(SRIDText+'-'+AxisShortName(2));
            If IncludeDatum Then
              AddFieldValue('Datum');
            OutputText := OutputText+LineEnding;
            OutputFile.Write(OutputText[1], Length(OutputText));
          End;
        { Output the data rows. }
        LastRecordIndex := InputData.RecordCount-1;
        For RecordIndex := 0 To LastRecordIndex Do
          Begin
            { Write the output line for the current record. }
            InputData.RecordNumber := RecordIndex;
            OutputText := InputData.RecordAsText(FieldTerminator, TextDelimiter);
            AddFieldValue(AxisCoordinateValue(0, OutputData[RecordIndex].Coordinates));
            AddFieldValue(AxisCoordinateValue(1, OutputData[RecordIndex].Coordinates));
            If IncludeZAxis Then
              AddFieldValue(AxisCoordinateValue(2, OutputData[RecordIndex].Coordinates));
            If IncludeDatum Then
              AddFieldValue(VerticalDataCodeToAbbreviation(OutputData[RecordIndex].DatumCode));
            OutputText := OutputText+LineEnding;
            OutputFile.WriteBuffer(OutputText[1], Length(OutputText));
            { Update progress display. }
            Progress.StepSave(RecordIndex);
          End;
        Progress.StopSave;
      Except
        On E:Exception Do
          Progress.Message('Insufficient memory to save data.');
      End;
  Finally
    OutputFile.Free;
    SetLength(OutputData, 0);
    InputData.Free;
    Progress.Free;
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

