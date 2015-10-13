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
  DataStreams, Progress, Settings, Options, About, Math, Geometry, Geodesy,
  GeomUtils, GeodUtils;

Type
  TMainForm = Class(TForm)
    DataBreak: TMenuItem;
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
    InputData: TDataStream;
    ProgressDisplay: TProgressDisplay;
    Function DataDrawGridCoordinates(Row: Integer): TCoordinates;
    Function DataLoaded: Boolean;
    Function TransformCoordinates(Const Coordinates: TCoordinates; InputIndex, OutputIndex: Integer): TCoordinates;
    Procedure ClearDataGrid;
    Procedure LocateOnMap(Const Coordinates: TCoordinates; CoordinateSystemIndex: Integer);
    Procedure DoInputValid(Sender: TObject);
    Procedure DoInputChangeSystem(Sender: TObject);
    Procedure DoOutputChangeSystem(Sender: TObject);
    Procedure DoLoadProgress(Sender: TObject; Progress: Integer);
    Procedure DoParseProgress(Sender: TObject; Progress: Integer);
  Public
    { Public declarations. }
    GlobeSystemIndex: Integer;
    InputSystemIndex: Integer;
    InputFirstFieldIndex: Integer;
    InputSecondFieldIndex: Integer;
    InputThirdFieldIndex: Integer;
    OutputSystemIndex: Integer;
    OutputCoordinates: Array Of TCoordinates;
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
  ProgressDisplay := CreateProgressDisplay();
  {$IFDEF Darwin}
    ManualFileName := Copy(Application.ExeName, 1, Pos('.app', Application.ExeName)+3);
  {$ELSE}
    ManualFileName := Application.ExeName;
  {$ENDIF}
  ManualFileName := ChangeFileExt(ManualFileName, '.pdf');
  ManualAction.Enabled := FileExists(ManualFileName);
  MainGlobe := TGlobeControl.Create(Self);
  MainGlobe.Align := alClient;
  MainGlobe.Parent := Self;
  { Panels must be created in reverse order for top alignment to work correctly. }
  OutputPanel := TCoordinatesEntryPanel.Create(SidePanel, ptOutput);
  InputPanel := TCoordinatesEntryPanel.Create(SidePanel, ptInput);
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
  GlobeSystemIndex := CoordinateSystems.FindEPSGNumber(4937); { Globe uses WGS84/ETRS89. }
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
      InputPanel.Hide;
      OutputPanel.Hide;
      SetupDataGrid;
      ProgressDisplay.Hide;
      ShowSettingsForm(InputData);
      If InputData.RecordCount>0 Then;
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
End;

Procedure TMainForm.DataDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
Var
  CellText: String;
  Function AxisColumnValue(Index: Integer; AxisOrder: TAxisOrder): String;
  Begin
    Case AxisTypeFromIndex(Index, AxisOrder) Of
    atXAxis: Result := FormatCoordinate(OutputCoordinates[aRow-1].X);
    atYAxis: Result := FormatCoordinate(OutputCoordinates[aRow-1].Y);
    atZAxis: Result := FormatCoordinate(OutputCoordinates[aRow-1].Z);
    End;
  End;
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
              CellText := AxisColumnValue(0, AxisOrder)
            Else If aCol=InputData.FieldCount+2 Then
              CellText := AxisColumnValue(1, AxisOrder)
            Else If aCol=InputData.FieldCount+3 Then
              CellText := AxisColumnValue(2, AxisOrder);
        TOverrideGrid(DataDrawGrid).DrawCellText(aCol, aRow, aRect, aState, CellText);
      End;
End;

Procedure TMainForm.DataDrawGridSelection(Sender: TObject; aCol, aRow: Integer);
Begin
  MainGlobe.ShowMarker := False;
  If DataLoaded Then
    If (InputFirstFieldIndex<>-1) And (InputSecondFieldIndex<>-1) Then
      If (aRow<>-1) And (aCol<InputData.FieldCount) Then
        LocateOnMap(DataDrawGridCoordinates(aRow), InputSystemIndex);
End;

Procedure TMainForm.SaveActionExecute(Sender: TObject);
Begin
  If SavePointsDialog.Execute Then
    Begin
      // TODO: Save data and transformation results.
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
      LastRecordIndex := InputData.RecordCount-1;
      For RecordIndex := 0 To LastRecordIndex Do
        Begin
          { Construct Input coordinates. }
          InputCoordinates := DataDrawGridCoordinates(RecordIndex+1);
          { Calculate the output coordinates}
          OutputCoordinates[RecordIndex] := TransformCoordinates(InputCoordinates, InputSystemIndex, OutputSystemIndex);
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
  DataDrawGrid.Hide;
  InputPanel.Show;
  OutputPanel.Show;
  MainGlobe.ShowMarker := False;
  SaveAction.Enabled := False;
  UnloadAction.Enabled := False;
  FreeAndNil(InputData);
  InputFirstFieldIndex := -1;
  InputSecondFieldIndex := -1;
  SetLength(OutputCoordinates, 0);
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

Procedure TMainForm.ClearActionExecute(Sender: TObject);
Begin
  InputPanel.Clear;
  OutputPanel.Clear;
End;

Procedure TMainForm.CutActionExecute(Sender: TObject);
Begin
  Clipboard.AsText := TEdit(Screen.ActiveControl).SelText;
  TEdit(Screen.ActiveControl).SelText := EmptyStr;
End;

Procedure TMainForm.DataSettingsActionExecute(Sender: TObject);
Begin
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
  { Strip out control and extended characters from the clipboard text. }
  RawText := Clipboard.AsText;
  For Index := 1 To Length(RawText) Do
    If (RawText[Index]<' ') Or (RawText[Index]>#127) Then
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

Function TMainForm.DataDrawGridCoordinates(Row: Integer): TCoordinates;
Begin
  InputData.RecordNumber := Row-1;
  { Construct Input coordinate. }
  Case CoordinateSystems.Items(InputSystemIndex).AxisOrder Of
  aoXYZ:
    Begin
      Result.X := StrToFloatDef(InputData.Fields[InputFirstFieldIndex], 0);
      Result.Y:= StrToFloatDef(InputData.Fields[InputSecondFieldIndex], 0);
      Result.Z := 0;
    End;
  aoYXZ:
    Begin
      Result.Y := StrToFloatDef(InputData.Fields[InputFirstFieldIndex], 0);
      Result.X := StrToFloatDef(InputData.Fields[InputSecondFieldIndex], 0);
      Result.Z := 0;
    End;
  End;
End;

Function TMainForm.DataLoaded: Boolean;
Begin
  Result := Assigned(InputData);
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

Function TMainForm.TransformCoordinates(Const Coordinates: TCoordinates; InputIndex, OutputIndex: Integer): TCoordinates;
Var
  InputCoordinates, GeocentricCoordinates: TCoordinates;
Begin
  If (InputIndex<>-1) And (OutputIndex<>-1) Then
    Begin
       If CoordinateSystems.Items(InputIndex).CoordinateType=ctGeodetic Then
         InputCoordinates := GeodeticDegToRad(Coordinates)
       Else
         InputCoordinates := Coordinates;
      GeocentricCoordinates := CoordinateSystems.Items(InputIndex).ConvertToGeocentric(InputCoordinates);
      Result := CoordinateSystems.Items(OutputIndex).ConvertFromGeocentric(GeocentricCoordinates);
      If CoordinateSystems.Items(OutputIndex).CoordinateType=ctGeodetic Then
        Result := GeodeticRadToDeg(Result);
    End;
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
  If OutputPanel.SelectedCoordinateSystemIndex<>-1 Then
    DoOutputChangeSystem(Self);
  { Display map preview location.}
  LocateOnMap(InputPanel.Coordinates, InputPanel.SelectedCoordinateSystemIndex);
End;

Procedure TMainForm.DoInputChangeSystem(Sender: TObject);
Begin
  OutputPanel.Clear;
End;

Procedure TMainForm.DoOutputChangeSystem(Sender: TObject);
Var
  InputIndex: Integer;
  OutputIndex: Integer;
Begin
  If InputPanel.Valid Then
    Begin
      InputIndex := InputPanel.SelectedCoordinateSystemIndex;
      OutputIndex := OutputPanel.SelectedCoordinateSystemIndex;
      OutputPanel.Coordinates := TransformCoordinates(InputPanel.Coordinates, InputIndex, OutputIndex);
    End;
End;

Procedure TMainForm.SetupDataGrid;
Var
  Col, LastCol: Integer;
  NewWidth, AlternativeWidth: Integer;
  EPSGText: String;
  Function AxisColumnShortName(Index: Integer): String;
  Begin
    With CoordinateSystems.Items(OutputSystemIndex) Do
      Case AxisTypeFromIndex(Index, AxisOrder) Of
      atXAxis: Result := AxisNames.ShortX;
      atYAxis: Result := AxisNames.ShortY;
      atZAxis: Result := AxisNames.ShortZ;
      End;
  End;
Begin
  DataDrawGrid.BeginUpdate;
  ClearDataGrid;
  DataDrawGrid.RowCount := InputData.RecordCount+1; { Records plus header row. }
  Canvas.Font := DataDrawGrid.Font;
  LastCol := InputData.FieldCount-1;
  For Col := 0 To LastCol Do
    With DataDrawGrid.Columns.Add Do
      Begin
        Title.Caption := InputData.Names[Col];
        { Calculate the width of the caption plus a couple of spaces. }
        NewWidth := Canvas.TextWidth('  '+Title.Caption);
        { Calculate the width of the first data item plus a couple of spaces. }
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
        EPSGText := IntToStr(EPSGNumber);
        With DataDrawGrid.Columns.Add Do
          Begin
            Title.Caption := EPSGText+'-'+AxisColumnShortName(0);
            Width := NewWidth;
          End;
        With DataDrawGrid.Columns.Add Do
          Begin
            Title.Caption := EPSGText+'-'+AxisColumnShortName(1);
            Width := NewWidth;
          End;
        { Add an elevation column if required. }
        If (InputThirdFieldIndex<>-1) Or (CoordinateType=ctGeocentric) Then
          With DataDrawGrid.Columns.Add Do
            Begin
              Title.Caption := EPSGText+'-'+AxisColumnShortName(2);
              Width := NewWidth;
            End;
      End;
  DataDrawGrid.Show;
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

End.

