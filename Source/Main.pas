Unit Main;

{ GridInQuest II Coordinate Transformation Utility Main Unit.

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
  Classes, SysUtils, FileUtil, LCLIntf, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, ActnList, StdCtrls, Grids, Clipbrd, GlobeCtrl, CoordCtrls,
  DataStreams, Progress, Settings, Options, About, Config, Geometry, Geodesy,
  GeodUtils, OSTab;

Type
  TMainForm = Class(TForm)
    TransformButton: TButton;
    ExchangeAction: TAction;
    DataBreak: TMenuItem;
    BottomPanel: TPanel;
    TransformMenuItem: TMenuItem;
    TransformAction: TAction;
    DataSettingsAction: TAction;
    ClearAction: TAction;
    CopyBreak: TMenuItem;
    CutMenuItem: TMenuItem;
    DataSettingsMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    CutBreak: TMenuItem;
    PasteAction: TAction;
    CopyAction: TAction;
    CutAction: TAction;
    ZoomInAction: TAction;
    ZoomOutAction: TAction;
    ReCenterAction: TAction;
    MapMenuItem: TMenuItem;
    ZoomInMenuItem: TMenuItem;
    ZoomOutMenuItem: TMenuItem;
    ReCentreBreak: TMenuItem;
    ReCenterMenuItem: TMenuItem;
    ManualAction: TAction;
    CopyInputAction: TAction;
    CopyOutputAction: TAction;
    OpenPointsDialog: TOpenDialog;
    OptionsAction: TAction;
    LoadAction: TAction;
    DataDrawGrid: TDrawGrid;
    SaveAction: TAction;
    SavePointsDialog: TSaveDialog;
    UnloadAction: TAction;
    UnloadMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    CopyInputMenuItem: TMenuItem;
    ManualMenuItem: TMenuItem;
    AboutBreak: TMenuItem;
    LoadMenuItem: TMenuItem;
    ExitBreak: TMenuItem;
    CopyOutputMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    OptionsBreak: TMenuItem;
    OptionsMenuItem: TMenuItem;
    ExitAction: TAction;
    AboutAction: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    SidePanel: TPanel;
    PanelSplitter: TSplitter;
    Procedure AboutActionExecute(Sender: TObject);
    Procedure ClearActionExecute(Sender: TObject);
    Procedure CopyActionExecute(Sender: TObject);
    Procedure CopyInputActionExecute(Sender: TObject);
    Procedure CopyOutputActionExecute(Sender: TObject);
    Procedure CutActionExecute(Sender: TObject);
    Procedure DataSettingsActionExecute(Sender: TObject);
    Procedure EditMenuItemClick(Sender: TObject);
    Procedure ExchangeActionExecute(Sender: TObject);
    Procedure ExitActionExecute(Sender: TObject);
    Procedure FileMenuItemClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure LoadActionExecute(Sender: TObject);
    Procedure ManualActionExecute(Sender: TObject);
    Procedure OptionsActionExecute(Sender: TObject);
    Procedure PasteActionExecute(Sender: TObject);
    Procedure DataDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    Procedure DataDrawGridSelection(Sender: TObject; aCol, aRow: Integer);
    Procedure ReCenterActionExecute(Sender: TObject);
    Procedure SaveActionExecute(Sender: TObject);
    Procedure TransformActionExecute(Sender: TObject);
    Procedure UnloadActionExecute(Sender: TObject);
    Procedure ZoomInActionExecute(Sender: TObject);
    Procedure ZoomOutActionExecute(Sender: TObject);
  Private
    { Private declarations. }
    ManualFileName: String;
    MainGlobe: TGlobeControl;
    InputPanel: TCoordinatesEntryPanel;
    OutputPanel: TCoordinatesEntryPanel;
    BlankPanel: TPanel;
    ProgressDisplay: TProgressDisplay;
    Function AxisShortName(SystemIndex, AxisIndex: Integer): String;
    Function DataDrawGridCoordinates(Row: Integer): TCoordinates;
    Function DataLoaded: Boolean;
    Function RecordOutputCoordinateText(RecordNumber, AxisIndex: Integer; AxisOrder: TAxisOrder; CoordinateType: TCoordinateType): String;
    Function TransformCoordinates(Const Coordinates: TCoordinates; InputIndex, OutputIndex: Integer): TCoordinates;
    Procedure ClearDataGrid;
    Procedure LocateOnMap(Const Coordinates: TCoordinates; CoordinateSystemIndex: Integer);
    Procedure DoInputValid(Sender: TObject);
    Procedure DoInputChangeSystem(Sender: TObject);
    Procedure DoOutputChangeSystem(Sender: TObject);
    Procedure DoLoadProgress(Sender: TObject; Progress: Integer);
    Procedure DoParseProgress(Sender: TObject; Progress: Integer);
    Procedure SetPanelFormattingOptions(DataPanel: TCoordinatesEntryPanel; Settings: TFormatSettings);
    Procedure CoordinateTypeAndSettingsToOptionsAndDecimalPlaces(Const CoordinateType: TCoordinateType; Const Settings: TFormatSettings;
                                                                 Out Options: TTypedOptions; Out DecimalPlaces, HeightDecimalPlaces: Integer);
  Public
    { Public declarations. }
    InputData: TDataStream;
    GlobeSystemIndex: Integer;
    InputSystemIndex: Integer;
    InputFirstFieldIndex: Integer;
    InputSecondFieldIndex: Integer;
    InputThirdFieldIndex: Integer;
    OutputSystemIndex: Integer;
    OutputCoordinates: Array Of TCoordinates;
    OutputData: Array Of TVerticalDatumCode;
    OutputFieldTerminator: Char;
    OutputTextDelimiter: Char;
    Procedure ClearOutputData;
    Procedure SetupDataGrid;
  End;

Var
  MainForm: TMainForm;

Implementation

{$R *.lfm}

{ Local Grid class to enable invocation of protected TCustomDrawGrid methods. }
Type
  TOverrideGrid = Class(TCustomDrawGrid)
  End;

Procedure TMainForm.FormCreate(Sender: TObject);
Begin
  ReadConfigOptions;
  If Screen.Width<1200 Then
    Width := Screen.Width;
  ProgressDisplay := CreateProgressDisplay();
  ManualFileName := ChangeFileExt(Application.ExeName, '.pdf');
  ManualAction.Enabled := FileExists(ManualFileName);
  MainGlobe := TGlobeControl.Create(Self);
  MainGlobe.Align := alClient;
  MainGlobe.Parent := Self;
  { Panels must be created in reverse order for top alignment to work correctly. }
  OutputPanel := TCoordinatesEntryPanel.Create(SidePanel, ptOutput);
  OutputPanel.Parent := SidePanel;
  InputPanel := TCoordinatesEntryPanel.Create(SidePanel, ptInput);
  InputPanel.Parent := SidePanel;
  BlankPanel := TPanel.Create(SidePanel);
  BlankPanel.Parent := SidePanel;
  BlankPanel.Align := alClient;
  {$IFDEF Windows}
  SidePanel.Font.Name := 'Arial';
  SidePanel.Font.Size := 12;
  {$ELSE}
    {$IFDEF Darwin}
    SidePanel.Font.Size := 14;
    {$ELSE}
    SidePanel.Font.Size := 10;
    {$ENDIF}
  {$ENDIF}
  InputPanel.TabOrder := 0;
  OutputPanel.TabOrder := 1;
  InputPanel.OnValid := @DoInputValid;
  InputPanel.OnChangeSystem := @DoInputChangeSystem;
  OutputPanel.OnChangeSystem := @DoOutputChangeSystem;
  InputData := Nil;
  InputSystemIndex := -1;
  InputFirstFieldIndex := -1;
  InputSecondFieldIndex := -1;
  InputThirdFieldIndex := -1;
  OutputSystemIndex := -1;
  GlobeSystemIndex := CoordinateSystems.FindSRIDNumber(4937); { Globe uses WGS84/ETRS89. }
End;

Procedure TMainForm.FormDestroy(Sender: TObject);
Begin
  If Assigned(InputData) Then
    InputData.Free;
  ProgressDisplay.Free;
End;

Procedure TMainForm.LoadActionExecute(Sender: TObject);
Begin
  If OpenPointsDialog.Execute Then
    Begin
      Screen.Cursor := crHourglass;
      InputData := TDataStream.Create;
      InputData.OnLoadProgress := @DoLoadProgress;
      InputData.OnParseProgress := @DoParseProgress;
      Try
        InputData.LoadFromFile(OpenPointsDialog.FileName);
      Except
        On E:Exception Do
          Begin
            ShowMessage('Insufficient memory to load data.');
            InputData.Free;
            ProgressDisplay.Hide;
            Screen.Cursor := crDefault;
            Exit;
          End;
      End;
      SaveAction.Enabled := True;
      UnloadAction.Enabled := True;
      DataSettingsAction.Enabled := True;
      TransformAction.Enabled := True;
      InputPanel.Hide;
      OutputPanel.Hide;
      SetupDataGrid;
      ProgressDisplay.Hide;
      ShowSettingsForm(InputData);
      If InputData.RecordCount>0 Then
        Begin
          DataDrawGrid.Row := 1;
          DataDrawGrid.TopRow := 1;
          DataDrawGridSelection(Self, 1, 1);
        End;
      Screen.Cursor := crDefault;
    End;
End;

Procedure TMainForm.ManualActionExecute(Sender: TObject);
Begin
  OpenDocument(ManualFileName);
End;

Procedure TMainForm.OptionsActionExecute(Sender: TObject);
Begin
  ShowOptionsForm;
  SetPanelFormattingOptions(InputPanel, InteractiveSettings);
  If OutputPanel.CoordinateSystemIndex=-1 Then
    SetPanelFormattingOptions(OutputPanel, InteractiveSettings)
  Else
    DoOutputChangeSystem(Self);
  DataDrawGrid.Refresh;
End;

Procedure TMainForm.DataDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
Var
  CellText: String;
Begin
  If aRow=0 Then
    DataDrawGrid.DefaultDrawCell(aCol, aRow, aRect, aState)
  Else
    If aCol>0 Then
      Begin
        If aCol<=InputData.FieldCount Then
          CellText := InputData.Values[aRow-1, aCol-1]
        Else
          With CoordinateSystems.Items(OutputSystemIndex) Do
            If aCol=InputData.FieldCount+1 Then
              CellText := RecordOutputCoordinateText(aRow-1, 0, AxisOrder, CoordinateType)
            Else If aCol=InputData.FieldCount+2 Then
              CellText := RecordOutputCoordinateText(aRow-1, 1, AxisOrder, CoordinateType)
            Else If aCol=InputData.FieldCount+3 Then
              CellText := RecordOutputCoordinateText(aRow-1, 2, AxisOrder, CoordinateType);
        TOverrideGrid(DataDrawGrid).DrawCellText(aCol, aRow, aRect, aState, CellText);
      End;
End;

Procedure TMainForm.DataDrawGridSelection(Sender: TObject; aCol, aRow: Integer);
Begin
  MainGlobe.ShowMarker := False;
  If DataLoaded Then
    If (InputFirstFieldIndex<>-1) And (InputSecondFieldIndex<>-1) Then
      If (aRow<>-1) Then
        LocateOnMap(DataDrawGridCoordinates(aRow), InputSystemIndex);
End;

Procedure TMainForm.SaveActionExecute(Sender: TObject);
Var
  OutputFile: TFileStream;
  OutputText: String;
  RecordIndex, LastRecordIndex: Integer;
  Function AddDelimiters(Text: String): String;
  Begin
    If OutputTextDelimiter=#0 Then
      Result := Text
    Else
      Result := OutputTextDelimiter+StringReplace(Text, OutputTextDelimiter, OutputTextDelimiter+OutputTextDelimiter, [rfReplaceAll])+OutputTextDelimiter;
  End;
Begin
  If Length(OutputCoordinates)=0 Then
    Begin
      ShowMessage('There is no transformed data to save.');
      Exit;
    End;
  If SavePointsDialog.Execute Then
    Try
      { Warn the user if the file already exists. }
      If FileExists(SavePointsDialog.FileName) Then
        If MessageDlg('This file already exists! Do you want to overwrite it?', mtWarning, [mbYes, mbNo],0) = mrNo Then
          Exit;
      Screen.Cursor := crHourglass;
      Try
        OutputFile := TFileStream.Create(SavePointsDialog.FileName, fmCreate);
        ProgressDisplay.Show('Saving Data');
        { Set delimiters as needed by output formats. }
        Case ExtractFileExt(SavePointsDialog.FileName) Of
        '.csv':
          Begin
            OutputFieldTerminator := ',';
            OutputTextDelimiter := '"';
          End;
        '.tab':
          Begin
            OutputFieldTerminator := #9;
            OutputTextDelimiter := #0;
          End;
        Else
          OutputFieldTerminator := ',';
          OutputTextDelimiter := #0;
        End;
        { Write the header line for the output file. }
        OutputText := InputData.NamesAsText(OutputFieldTerminator, OutputTextDelimiter);
        With CoordinateSystems.Items(OutputSystemIndex) Do
          Begin
            OutputText := OutputText+OutputFieldTerminator;
            OutputText := OutputText+AddDelimiters(Abbreviation+'-'+AxisShortName(OutputSystemIndex, 0));
            OutputText := OutputText+OutputFieldTerminator;
            OutputText := OutputText+AddDelimiters(Abbreviation+'-'+AxisShortName(OutputSystemIndex, 1));
            { Output the third coordinate name if needed. }
            If (InputThirdFieldIndex<>-1) Or (CoordinateType=ctCartesian) Then
              Begin
                OutputText := OutputText+OutputFieldTerminator;
                OutputText := OutputText+AddDelimiters(Abbreviation+'-'+AxisShortName(OutputSystemIndex, 2));
              End;
            OutputText := OutputText+LineEnding;
          End;
        OutputFile.Write(OutputText[1], Length(OutputText));
        LastRecordIndex := InputData.RecordCount-1;
        For RecordIndex := 0 To LastRecordIndex Do
          Begin
            { Write the output line for the current record. }
            InputData.RecordNumber := RecordIndex;
            OutputText := InputData.RecordAsText(OutputFieldTerminator, OutputTextDelimiter);
            With CoordinateSystems.Items(OutputSystemIndex) Do
              Begin
                OutputText := OutputText+OutputFieldTerminator;
                OutputText := OutputText+AddDelimiters(RecordOutputCoordinateText(RecordIndex, 0, AxisOrder, CoordinateType));
                OutputText := OutputText+OutputFieldTerminator;
                OutputText := OutputText+AddDelimiters(RecordOutputCoordinateText(RecordIndex, 1, AxisOrder, CoordinateType));
                { Output the third coordinate if needed. }
                If (InputThirdFieldIndex<>-1) Or (CoordinateType=ctCartesian) Then
                  Begin
                    OutputText := OutputText+OutputFieldTerminator;
                    OutputText := OutputText+AddDelimiters(RecordOutputCoordinateText(RecordIndex, 2, AxisOrder, CoordinateType));
                  End;
                OutputText := OutputText+LineEnding;
              End;
            OutputFile.Write(OutputText[1], Length(OutputText));
            { Update progress display. }
            ProgressDisplay.Progress := Integer(Int64(100*Int64(RecordIndex)) Div LastRecordIndex);
          End;
      Except
        On E:Exception Do
          ShowMessage('Insufficient memory to save data.');
      End;
    Finally
      OutputFile.Free;
      ProgressDisplay.Hide;
      Screen.Cursor := crDefault;
    End;
End;

Procedure TMainForm.TransformActionExecute(Sender: TObject);
Var
  RecordIndex, LastRecordIndex: Integer;
  InputCoordinates: TCoordinates;
Begin
  If DataLoaded Then
    Begin
      If InputSystemIndex=-1 Then
        Begin
          ShowMessage('There is no input coordinate system selected.');
          Exit;
        End;
      If (InputFirstFieldIndex=-1) Or (InputSecondFieldIndex=-1) Then
        Begin
          ShowMessage('There are no input coordinate fields selected.');
          Exit;
        End;
      If OutputSystemIndex=-1 Then
        Begin
          ShowMessage('There is no output coordinate system selected.');
          Exit;
        End;
      Screen.Cursor := crHourglass;
      ProgressDisplay.Show('Transforming Data');
      SetLength(OutputCoordinates, InputData.RecordCount);
      SetLength(OutputData, InputData.RecordCount);
      LastRecordIndex := InputData.RecordCount-1;
      For RecordIndex := 0 To LastRecordIndex Do
        Begin
          { Construct Input coordinates. }
          InputCoordinates := DataDrawGridCoordinates(RecordIndex+1);
          { Calculate the output coordinates}
          OutputCoordinates[RecordIndex] := TransformCoordinates(InputCoordinates, InputSystemIndex, OutputSystemIndex);
          OutputData[RecordIndex] := CoordinateSystems.Items(OutputSystemIndex).LastVerticalDatum;
          { Update progress display. }
          ProgressDisplay.Progress := Integer(Int64(100*Int64(RecordIndex)) Div LastRecordIndex);
        End;
      ProgressDisplay.Hide;
      SetupDataGrid;
      Screen.Cursor := crDefault;
    End;
End;

Procedure TMainForm.UnloadActionExecute(Sender: TObject);
Begin
  BottomPanel.Hide;
  DataDrawGrid.Hide;
  InputPanel.Show;
  OutputPanel.Show;
  MainGlobe.ShowMarker := False;
  SaveAction.Enabled := False;
  UnloadAction.Enabled := False;
  FreeAndNil(InputData);
  InputFirstFieldIndex := -1;
  InputSecondFieldIndex := -1;
  ClearOutputData;
End;

Procedure TMainForm.ExitActionExecute(Sender: TObject);
Begin
  Close;
End;

Procedure TMainForm.FileMenuItemClick(Sender: TObject);
Begin
  LoadAction.Enabled := Not DataLoaded;
  SaveAction.Enabled := DataLoaded;
  UnloadAction.Enabled := DataLoaded;
  DataSettingsAction.Enabled := DataLoaded;
  TransformAction.Enabled := DataLoaded;
End;

Procedure TMainForm.EditMenuItemClick(Sender: TObject);
Begin
  ClearAction.Enabled := Not DataLoaded;
  CutAction.Enabled := (Screen.ActiveControl Is TEdit) And
                       (TEdit(Screen.ActiveControl).SelText<>EmptyStr);
  CopyAction.Enabled := CutAction.Enabled;
  PasteAction.Enabled := (Screen.ActiveControl Is TEdit) And
                         (Length(Clipboard.AsText)>0);
  CopyInputAction.Enabled := InputPanel.Valid;
  CopyOutputAction.Enabled := OutputPanel.Valid;
End;

Procedure TMainForm.ExchangeActionExecute(Sender: TObject);
Var
  SystemIndex: Integer;
  Coordinates: TCoordinates;
Begin
  SystemIndex := OutputPanel.CoordinateSystemIndex;
  Coordinates := OutputPanel.Coordinates;
  ClearAction.Execute;
  InputPanel.CoordinateSystemIndex := SystemIndex;
  InputPanel.Coordinates := Coordinates;
End;

Procedure TMainForm.ClearActionExecute(Sender: TObject);
Begin
  InputPanel.Clear;
  OutputPanel.Clear;
  MainGlobe.ShowMarker := False;
  Application.ProcessMessages;
End;

Procedure TMainForm.CutActionExecute(Sender: TObject);
Begin
  Clipboard.AsText := TEdit(Screen.ActiveControl).SelText;
  TEdit(Screen.ActiveControl).SelText := EmptyStr;
End;

Procedure TMainForm.DataSettingsActionExecute(Sender: TObject);
Begin
  { If output data has been generated, warn the user about its removal. }
  If Length(OutputCoordinates)>0 Then
    If MessageDlg('Confirmation', 'Transformed data must be cleared before settings can be changed.'+LineEnding+
                  'Do you want to continue?',
                  mtConfirmation, [mbYes, mbNo], 0)=mrYes Then
      Begin
        ClearOutputData;
        SetupDataGrid;
      End
    Else
      Exit;
  ShowSettingsForm(InputData);
End;

Procedure TMainForm.CopyActionExecute(Sender: TObject);
Begin
  Clipboard.AsText := TEdit(Screen.ActiveControl).SelText;
End;

Procedure TMainForm.PasteActionExecute(Sender: TObject);
Var
  RawText: String;
  Index: Integer;
Begin
  { Strip out control and extended characters from the clipboard text }
  { except for #194,#176 which is the byte sequence for the UTF8 degree symbol }
  { and #176 which is the ANSI degree symbol in most ISO/IEC 8859 standard codepages. }
  RawText := Clipboard.AsText;
  For Index := 1 To Length(RawText) Do
    If RawText[Index]=#194 Then
      Begin
        If Index=Length(RawText) Then
          RawText[Index] := ' '
        Else
          If RawText[Index+1]<>#176 Then
            Begin
              RawText[Index] := ' ';
              RawText[Index+1] := ' ';
            End;
      End
    Else
      If Not (RawText[Index] In [' '..#127, #176]) Then
        RawText[Index] := ' ';
  TEdit(Screen.ActiveControl).SelText := RawText;
End;

Procedure TMainForm.CopyInputActionExecute(Sender: TObject);
Begin
  Clipboard.AsText := InputPanel.CoordinatesAsText;
End;

Procedure TMainForm.CopyOutputActionExecute(Sender: TObject);
Begin
  Clipboard.AsText := OutputPanel.CoordinatesAsText;
End;

Procedure TMainForm.ZoomInActionExecute(Sender: TObject);
Begin
  MainGlobe.ZoomIn;
End;

Procedure TMainForm.ZoomOutActionExecute(Sender: TObject);
Begin
  MainGlobe.ZoomOut;
End;

Procedure TMainForm.ReCenterActionExecute(Sender: TObject);
Begin
  MainGlobe.ReCenter;
End;

Procedure TMainForm.AboutActionExecute(Sender: TObject);
Begin
  ShowAboutForm();
End;

Function TMainForm.AxisShortName(SystemIndex, AxisIndex: Integer): String;
Begin
  With CoordinateSystems.Items(SystemIndex) Do
    Case AxisTypeFromIndex(AxisIndex, AxisOrder) Of
    atXAxis: Result := AxisNames.ShortX;
    atYAxis: Result := AxisNames.ShortY;
    atZAxis: Result := AxisNames.ShortZ;
    End;
End;

Function TMainForm.DataDrawGridCoordinates(Row: Integer): TCoordinates;
Function TryTextToCoordinate(Text: String; CoordinateType: TCoordinateType; AxisType: TAxisType; Var Coordinate: TCoordinate): Boolean;
Begin
  Case CoordinateType Of
  ctCartesian:
    Result := TryCartesianTextToCoordinate(Text, Coordinate, AxisType);
  ctGeodetic:
    Result := TryGeodeticTextToCoordinate(Text, Coordinate, AxisType);
  ctProjected:
    Result := TryProjectedTextToCoordinate(Text, Coordinate, AxisType);
  Else
    Result := False;
  End;
End;
Begin
  InputData.RecordNumber := Row-1;
  { Construct Input coordinate. }
  With CoordinateSystems.Items(InputSystemIndex) Do
    Begin
      Case AxisOrder Of
      aoXYZ:
        Begin
          TryTextToCoordinate(InputData.Fields[InputFirstFieldIndex], CoordinateType, atXAxis, Result.X);
          TryTextToCoordinate(InputData.Fields[InputSecondFieldIndex], CoordinateType, atYAxis, Result.Y);
        End;
      aoYXZ:
        Begin
          TryTextToCoordinate(InputData.Fields[InputFirstFieldIndex], CoordinateType, atYAxis, Result.Y);
          TryTextToCoordinate(InputData.Fields[InputSecondFieldIndex], CoordinateType, atXAxis, Result.X);
        End;
      End;
      { Output the third coordinate name if needed. }
      If InputThirdFieldIndex<>-1 Then
        TryTextToCoordinate(InputData.Fields[InputThirdFieldIndex], CoordinateType, atZAxis, Result.Z)
      Else
        Result.Z := 0;
    End;
End;

Function TMainForm.DataLoaded: Boolean;
Begin
  Result := Assigned(InputData);
End;

Function TMainForm.RecordOutputCoordinateText(RecordNumber, AxisIndex: Integer; AxisOrder: TAxisOrder; CoordinateType: TCoordinateType): String;
Var
  AxisType: TAxisType;
  DecimalPlaces: Integer;
  HeightDecimalPlaces: Integer;
  Options: TTypedOptions;
Begin
  CoordinateTypeAndSettingsToOptionsAndDecimalPlaces(CoordinateType, OutputSettings, Options, DecimalPlaces, HeightDecimalPlaces);
  AxisType := AxisTypeFromIndex(AxisIndex, AxisOrder);
  If (AxisType=atZAxis) And (CoordinateType<>ctCartesian) Then { Use the HeightDecimalPlaces for non-cartesian Z axes. }
    DecimalPlaces := HeightDecimalPlaces;
  Case AxisType Of
  atXAxis: Result := FormatTypedCoordinate(OutputCoordinates[RecordNumber].X, CoordinateType, AxisType, DecimalPlaces, Options);
  atYAxis: Result := FormatTypedCoordinate(OutputCoordinates[RecordNumber].Y, CoordinateType, AxisType, DecimalPlaces, Options);
  atZAxis: Result := FormatTypedCoordinate(OutputCoordinates[RecordNumber].Z, CoordinateType, AxisType, DecimalPlaces, Options,
                                           VerticalDataCodeToAbbreviation(OutputData[RecordNumber]));
  End;
End;

Function TMainForm.TransformCoordinates(Const Coordinates: TCoordinates; InputIndex, OutputIndex: Integer): TCoordinates;
Var
  InputCoordinates, GeocentricCoordinates: TCoordinates;
  InputCoordinateSystemPointer: TCoordinateSystemPointer;
  OutputCoordinateSystemPointer: TCoordinateSystemPointer;
Begin
  If (InputIndex<>-1) And (OutputIndex<>-1) Then
    Begin
      InputCoordinateSystemPointer := CoordinateSystems.Pointers(InputIndex);
      OutputCoordinateSystemPointer := CoordinateSystems.Pointers(OutputIndex);
      If InputCoordinateSystemPointer^.CoordinateType=ctGeodetic Then
        InputCoordinates := GeodeticDegToRad(Coordinates)
      Else
        InputCoordinates := Coordinates;
      Try
        GeocentricCoordinates := InputCoordinateSystemPointer^.ConvertToGeocentric(InputCoordinates);
        If GeocentricCoordinates=NullCoordinates Then
          Result := NullCoordinates
        Else
          Result := OutputCoordinateSystemPointer^.ConvertFromGeocentric(GeocentricCoordinates);
        If OutputCoordinateSystemPointer^.CoordinateType=ctGeodetic Then
          Result := GeodeticRadToDeg(Result);
      Except
        Result := NullCoordinates;
      End;
    End;
End;

Procedure TMainForm.ClearDataGrid;
Begin
  DataDrawGrid.Columns.Clear;
  DataDrawGrid.RowCount := 1;
  DataDrawGrid.FixedRows := 1;
  DataDrawGrid.FixedCols := 1;
  DataDrawGrid.Row := 0;
  DataDrawGrid.Col := 0;
End;

Procedure TMainForm.ClearOutputData;
Begin
  SetLength(OutputCoordinates, 0);
  SetLength(OutputData, 0);
End;

Procedure TMainForm.LocateOnMap(Const Coordinates: TCoordinates; CoordinateSystemIndex: Integer);
  Procedure SetMapLocation(Const Coordinates: TCoordinates);
  Begin
    With MainGlobe Do
      Begin
        Marker.Lat := Coordinates.Latitude;
        Marker.Lon := Coordinates.Longitude;
        ShowMarker := True;
        Refresh;
      End;
  End;
Begin
  { No trasformation is required if the input coordinate system is already in ETRS89 geodetic. }
  If CoordinateSystemIndex=GlobeSystemIndex Then
    SetMapLocation(Coordinates)
  Else
    { If there is an input coordinate system selected, calculate the required transformation. }
    If CoordinateSystemIndex<>-1 Then
      SetMapLocation(TransformCoordinates(Coordinates, CoordinateSystemIndex, GlobeSystemIndex));
End;

Procedure TMainForm.DoInputValid(Sender: TObject);
Begin
  { If there is an output coordinate system selected, perform the conversion. }
  If OutputPanel.CoordinateSystemIndex<>-1 Then
    DoOutputChangeSystem(Self);
  { Display map preview location.}
  LocateOnMap(InputPanel.Coordinates, InputPanel.CoordinateSystemIndex);
End;

Procedure TMainForm.DoInputChangeSystem(Sender: TObject);
Begin
  SetPanelFormattingOptions(InputPanel, InteractiveSettings);
  OutputPanel.Clear(InputPanel.CoordinateSystemIndex);
  With MainGlobe Do
    Begin
      ShowMarker := False;
      Refresh;
    End;
End;

Procedure TMainForm.DoOutputChangeSystem(Sender: TObject);
Var
  InputIndex: Integer;
  OutputIndex: Integer;
  NewCoordinates: TCoordinates;
Begin
  SetPanelFormattingOptions(OutputPanel, InteractiveSettings);
  If InputPanel.Valid Then
    Begin
      InputIndex := InputPanel.CoordinateSystemIndex;
      OutputIndex := OutputPanel.CoordinateSystemIndex;
      NewCoordinates := TransformCoordinates(InputPanel.Coordinates, InputIndex, OutputIndex);
      OutputPanel.VerticalDatum := CoordinateSystems.Items(OutputIndex).LastVerticalDatum;
      OutputPanel.Coordinates := NewCoordinates;
    End
  Else
    OutputPanel.ClearCoordinates;
End;

Procedure TMainForm.SetupDataGrid;
Var
  Col, LastCol: Integer;
  NewWidth, AlternativeWidth: Integer;
Begin
  DataDrawGrid.BeginUpdate;
  ClearDataGrid;
  DataDrawGrid.RowCount := InputData.RecordCount+1; { Records plus header row. }
  LastCol := InputData.FieldCount-1;
  For Col := 0 To LastCol Do
    With DataDrawGrid.Columns.Add Do
      Begin
        Title.Caption := InputData.Names[Col];
        { Calculate the width of the caption plus a couple of spaces. }
        Canvas.Font := DataDrawGrid.TitleFont;
        NewWidth := Canvas.TextWidth('  '+Title.Caption);
        { Calculate the width of the first data item plus a couple of spaces. }
        Canvas.Font := DataDrawGrid.Font;
        AlternativeWidth := Canvas.TextWidth('  '+InputData.Values[0, Col]);
        { Choose the wider of the two widths. }
        If AlternativeWidth>NewWidth Then
          Width := AlternativeWidth
        Else
          Width := NewWidth;
      End;
  { If output coodinates have been generated. }
  If Length(OutputCoordinates)>0 Then
    With CoordinateSystems.Items(OutputSystemIndex) Do
      Begin
        { Calculate the width of a 10 character column. }
        NewWidth := Canvas.TextWidth('0123456789');
        With DataDrawGrid.Columns.Add Do
          Begin
            Title.Caption := Abbreviation+'-'+AxisShortName(OutputSystemIndex, 0);
            Width := NewWidth;
          End;
        With DataDrawGrid.Columns.Add Do
          Begin
            Title.Caption := Abbreviation+'-'+AxisShortName(OutputSystemIndex, 1);
            Width := NewWidth;
          End;
        { Add an elevation column if required. }
        If (InputThirdFieldIndex<>-1) Or (CoordinateType=ctCartesian) Then
          With DataDrawGrid.Columns.Add Do
            Begin
              Title.Caption := Abbreviation+'-'+AxisShortName(OutputSystemIndex, 2);
              Width := NewWidth;
            End;
      End;
  DataDrawGrid.Show;
  BottomPanel.Show;
  DataDrawGrid.EndUpdate;
End;

Procedure TMainForm.DoLoadProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    ProgressDisplay.Show('Load Data Progress')
  Else
    ProgressDisplay.Progress := Progress;
End;

Procedure TMainForm.DoParseProgress(Sender: TObject; Progress: Integer);
Begin
  If Progress=0 Then
    ProgressDisplay.Show('Parse Data Progress')
  Else
    ProgressDisplay.Progress := Progress;
End;

Procedure TMainForm.SetPanelFormattingOptions(DataPanel: TCoordinatesEntryPanel; Settings: TFormatSettings);
Var
  DecimalPlaces: Integer;
  HeightDecimalPlaces: Integer;
  Options: TTypedOptions;
Begin
  CoordinateTypeAndSettingsToOptionsAndDecimalPlaces(DataPanel.CoordinateType, Settings, Options, DecimalPlaces, HeightDecimalPlaces);
  DataPanel.Options := Options;
  DataPanel.DecimalPlaces := DecimalPlaces;
  DataPanel.HeightDecimalPlaces := HeightDecimalPlaces;
  DataPanel.Refresh;
End;

Procedure TMainForm.CoordinateTypeAndSettingsToOptionsAndDecimalPlaces(Const CoordinateType: TCoordinateType; Const Settings: TFormatSettings; Out Options: TTypedOptions; Out DecimalPlaces, HeightDecimalPlaces: Integer);
Begin
  Options := [];
  Case CoordinateType Of
  ctGeodetic:
    Begin
      Case Settings.GeodeticStyle Of
      'Degrees, Minutes, Seconds': Options := Options+[toThreePartSexagseimal];
      'Degrees, Decimal Minutes': Options := Options+[toTwoPartSexagseimal];
      End;
      Case Settings.GeodeticUnits Of
      'Symbols': Options := Options+[toGeodeticSymbols];
      'Letters': Options := Options+[toGeodeticLetters];
      End;
      Case Settings.GeodeticQuadrants Of
      'Signs': Options := Options+[toSignPrefix];
      'Letters': Options := Options+[toLetterSuffix];
      End;
      If Settings.GeodeticCompactFormat Then
        Options := Options+[toCompactWhitespace];
      If Settings.GeodeticPositiveLongitude Then
        Options := Options+[toPositiveLongitude];
      DecimalPlaces := Settings.GeodeticDecimalPlaces;
    End;
  ctProjected:
    Begin
      Case Settings.ProjectedStyle Of
      'Axis Prefix Letter': Options := Options+[toLetterPrefix];
      'Axis Suffix Letter': Options := Options+[toLetterSuffix];
      End;
      DecimalPlaces := Settings.ProjectedDecimalPlaces;
    End;
  ctCartesian:
    Begin
      Case Settings.CartesianStyle Of
      'Axis Prefix Letter': Options := Options+[toLetterPrefix];
      'Axis Suffix Letter': Options := Options+[toLetterSuffix];
      End;
      DecimalPlaces := Settings.CartesianDecimalPlaces;
    End;
  End;
  If Settings.HeightStyle='Value and Units' Then
    Options := Options+[toUnitSuffix];
  HeightDecimalPlaces := Settings.HeightDecimalPlaces;
  If Settings.HeightDatumSuffix Then
    Options := Options+[toHeightDatumSuffix];
End;

End.

