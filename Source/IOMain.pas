Unit IOMain;

{ File IO Processor Main Unit.

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
  Classes, SysUtils, XMLConf, Geometry, Geodesy, GeodProc, GeodUtils, GeomUtils,
  ETRS, BNG, ITM, IG, OSGeoid, DataStreams;

Type
  TSettingsInfo = Record
    NameRow: Integer;
    FirstRow: Integer;
    LastRow: Integer;
    BreaksText: String;
    ConsecutiveDelimiters: Boolean;
    FieldTerminator: Char;
    FormatType: TFormatType;
    IncludeDatum: Boolean;
    IncludeNames: Boolean;
    PreferredDatumCode: TVerticalDatumCode;
    TextDelimiter: Char;
    SourceSRID: Integer;
    SourceRevision: Integer;
    SourceXIndex: Integer;
    SourceYIndex: Integer;
    SourceZIndex: Integer;
    TargetSRID: Integer;
    TargetRevision: Integer;
    TargetXName: String;
    TargetYName: String;
    TargetZName: String;
    HorizontalDecimalPlaces: Integer;
    VerticalDecimalPlaces: Integer;
    ZeroFill: Boolean;
  End;

Function LoadSettings(LoadFileName: String; Var Settings: TSettingsInfo; RequireSRIDs: Boolean = True): Boolean;
Procedure ProcessFile(Var SettingsInfo: TSettingsInfo; Const InputFileName: String;
                      Const OutputFileName: String; ShowProgress: Boolean);

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
  TOutputItem = Record
    Coordinates: TCoordinates;
    DatumCode: TVerticalDatumCode;
  End;
  TOutputData = Array Of TOutputItem;

Function LoadSettings(LoadFileName: String; Var Settings: TSettingsInfo; RequireSRIDs: Boolean = True): Boolean;
Var
  XMLSettings: TXMLConfig;
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
          If RequireSRIDs Then
            If SourceSRID=0 Then
              Exit;
        CloseKey;
        OpenKey(XColumnKey);
          SourceXIndex := GetValue(ValueKey, 0)-1;
          If SourceXIndex=-1 Then
            Exit;
        CloseKey;
        OpenKey(YColumnKey);
          SourceYIndex := GetValue(ValueKey, 0)-1;
          If SourceYIndex=-1 Then
            Exit;
        CloseKey;
        OpenKey(ZColumnKey);
          SourceZIndex := GetValue(ValueKey, 0)-1;
        CloseKey;
      CloseKey;
      { Read output settings. }
      OpenKey(OutputKey);
        OpenKey(SRIDNumberKey);
          TargetSRID := GetValue(ValueKey, 0);
          If RequireSRIDs Then
            If TargetSRID=0 Then
              Exit;
        CloseKey;
        OpenKey(NameRowKey);
          IncludeNames := (GetValue(ValueKey, 1)<>0); { Default to including names. }
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

Procedure ProcessFile(Var SettingsInfo: TSettingsInfo; Const InputFileName: String;
                      Const OutputFileName: String; ShowProgress: Boolean);
Var
  Progress: TProgressHandler;
  InputData: TDataStream;
  RecordIndex: Integer;
  LastRecordIndex: Integer;
  InputCoordinates: TCoordinates;
  GlobalCoordinates: TCoordinates;
  OutputData: TOutputData;
  OutputFile: TFileStream;
  OutputText: String;
  SourceSystemPointer: TCoordinateSystemPointer;
  TargetSystemPointer: TCoordinateSystemPointer;
  IncludeXYAxes: Boolean;
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
    { Construct Input coordinate. }
    With InputData, SettingsInfo, SourceSystemPointer^ Do
      Begin
        { Locate the required record. }
        RecordNumber := Index;
        { Convert the X and Y coordinates. }
        TryTextToCoordinate(Fields[SourceXIndex], Result.X, CoordinateType, atXAxis);
        TryTextToCoordinate(Fields[SourceYIndex], Result.Y, CoordinateType, atYAxis);
        { Convert the Z coordinate if needed. }
        If SourceZIndex<>-1 Then
          TryTextToCoordinate(Fields[SourceZIndex], Result.Z, CoordinateType, atZAxis)
        Else
          Result.Z := 0;
      End;
  End;
  Function AxisCoordinateValue(AxisIndex: Integer; Coordinates: TCoordinates): String;
  Begin
    With TargetSystemPointer^, SettingsInfo Do
      Case AxisTypeFromIndex(AxisIndex, AxisOrder) Of
      atXAxis: Result := FormatCoordinate(Coordinates.X, HorizontalDecimalPlaces, ZeroFill);
      atYAxis: Result := FormatCoordinate(Coordinates.Y, HorizontalDecimalPlaces, ZeroFill);
      atZAxis: Result := FormatCoordinate(Coordinates.Z, VerticalDecimalPlaces, ZeroFill);
      End;
  End;
  Procedure AddFieldValue(Text: String);
  Begin
    With SettingsInfo Do
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
      InputData.FormatType := SettingsInfo.FormatType;
      If InputData.FormatType=ftFixed Then
        ParseBreaksList(SettingsInfo.BreaksText);
      InputData.FirstRow := SettingsInfo.FirstRow;
      InputData.LastRow := SettingsInfo.LastRow;
      InputData.NameRow := SettingsInfo.NameRow;
      InputData.FieldTerminator := SettingsInfo.FieldTerminator;
      InputData.TextDelimiter := SettingsInfo.TextDelimiter;
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
    SourceSystemPointer := SRIDToSystemPointer(SettingsInfo.SourceSRID, SettingsInfo.SourceRevision);
    TargetSystemPointer := SRIDToSystemPointer(SettingsInfo.TargetSRID, SettingsInfo.TargetRevision);
    SourceIsGeodetic := IsSRIDGeodeticSystem(SettingsInfo.SourceSRID, SettingsInfo.TargetRevision);
    TargetIsGeodetic := IsSRIDGeodeticSystem(SettingsInfo.TargetSRID, SettingsInfo.TargetRevision);
    TargetSystemPointer^.PreferredVerticalDatum := SettingsInfo.PreferredDatumCode;
    { Set the default default target field names if required. }
    With SettingsInfo, TargetSystemPointer^ Do
      Begin
        If TargetXName=EmptyStr Then
          TargetXName := Abbreviation+'-'+AxisNames.ShortX;
        If TargetYName=EmptyStr Then
          TargetYName := Abbreviation+'-'+AxisNames.ShortY;
        If TargetZName=EmptyStr Then
          TargetZName := Abbreviation+'-'+AxisNames.ShortZ;
      End;
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
        GlobalCoordinates := SourceSystemPointer^.ConvertToGlobal(InputCoordinates);
        { Calculate the output coordinates}
        With OutputData[RecordIndex] Do
          Begin
            Case SourceSystemPointer^.GlobalType Of
            gtCartesian:
              If TargetSystemPointer^.GlobalType=gtGeodetic Then
                GlobalCoordinates := GeocentricToGeodetic(GlobalCoordinates, GRS80Ellipsoid);
            gtGeodetic:
              If TargetSystemPointer^.GlobalType=gtCartesian Then
                GlobalCoordinates := GeodeticToGeocentric(GlobalCoordinates, GRS80Ellipsoid);
            End;
            Coordinates := TargetSystemPointer^.ConvertFromGlobal(GlobalCoordinates);
            If TargetIsGeodetic Then
              Coordinates := GeodeticDegToRad(Coordinates);
            DatumCode := TargetSystemPointer^.LastVerticalDatum;
          End;
        { Update progress display. }
        Progress.StepTransform(RecordIndex);
      End;
    Progress.StopTransform;
    Progress.Message('Writing output to: '+OutputFileName);
    With SettingsInfo, TargetSystemPointer^ Do
      Try
        IncludeXYAxes := (TargetXName<>'') And (TargetYName<>'');
        IncludeZAxis := (TargetZName<>'') And ((SourceZIndex<>-1) Or (CoordinateType=ctCartesian));
        Progress.StartSave(InputData.RecordCount);
        OutputFile := TFileStream.Create(OutputFileName, fmCreate);
        { Write the header line for the output file. }
        If IncludeNames Then
          Begin
            OutputText := InputData.NamesAsText(FieldTerminator, TextDelimiter);
            { Output the horizontal axes names. }
            If IncludeXYAxes Then
              Case AxisOrder Of
              aoXYZ:
                Begin
                  AddFieldValue(TargetXName);
                  AddFieldValue(TargetYName);
                End;
              aoYXZ:
                Begin
                  AddFieldValue(TargetYName);
                  AddFieldValue(TargetXName);
                End;
              End;
            { Output the third coordinate axis name if needed. }
            If IncludeZAxis Then
              AddFieldValue(TargetZName);
            { Output the datum column name. }
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
            If IncludeXYAxes Then
              Begin
                AddFieldValue(AxisCoordinateValue(0, OutputData[RecordIndex].Coordinates));
                AddFieldValue(AxisCoordinateValue(1, OutputData[RecordIndex].Coordinates));
              End;
            If IncludeZAxis Then
              AddFieldValue(AxisCoordinateValue(2, OutputData[RecordIndex].Coordinates));
            If IncludeDatum Then
              AddFieldValue(VerticalDatumCodeToAbbreviation(OutputData[RecordIndex].DatumCode));
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

End.

