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
  ExtCtrls, Menus, ActnList, StdCtrls, Grids, Clipbrd, Geometry, Geodesy,
  GlobeCtrl, CoordCtrls, DataStreams, Progress, Settings, Options, About;

Type
  TMainForm = Class(TForm)
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
    Function DataLoaded: Boolean;
    Procedure ClearDataGrid;
    Procedure DoInputValid(Sender: TObject);
    Procedure DoOutputChangeSystem(Sender: TObject);
    Procedure SetupDataGrid;
    Procedure DoLoadProgress(Sender: TObject; Progress: Integer);
    Procedure DoParseProgress(Sender: TObject; Progress: Integer);
  Public
    { Public declarations. }
    // TODO: Need to store more comprehensive transform settings.
    InputLatIndex: Integer;
    InputLonIndex: Integer;
    InputAltIndex: Integer;
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
  OutputPanel.OnChangeSystem := @DoOutputChangeSystem;
  InputData := Nil;
  InputLatIndex := -1;
  InputLonIndex := -1;
  InputAltIndex := -1;
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
Begin
  If aRow=0 Then
    DataDrawGrid.DefaultDrawCell(aCol, aRow, aRect, aState)
  Else
    If aCol>0 Then
      Begin
        CellText := InputData.Values[aRow-1, aCol-1];
        TOverrideGrid(DataDrawGrid).DrawCellText(aCol, aRow, aRect, aState, CellText);
      End;
End;

Procedure TMainForm.DataDrawGridSelection(Sender: TObject; aCol, aRow: Integer);
Begin
  MainGlobe.ShowMarker := False;
  If DataLoaded Then
    If (InputLatIndex<>-1) And (InputLonIndex<>-1) Then
      If (aRow<>-1) And (aCol<InputData.FieldCount) Then
        Begin
          InputData.RecordNumber := aRow-1;
          MainGlobe.Marker.Lat := StrToFloatDef(InputData.Fields[InputLatIndex], 0);
          MainGlobe.Marker.Lon := StrToFloatDef(InputData.Fields[InputLonIndex], 0);
          MainGlobe.ShowMarker := True;
          MainGlobe.Refresh;
        End;
End;

Procedure TMainForm.SaveActionExecute(Sender: TObject);
Begin
  If SavePointsDialog.Execute Then
    Begin
      // TODO: Save data and transformation results.
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
  InputLatIndex := -1;
  InputLonIndex := -1;
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
Begin
  TEdit(Screen.ActiveControl).SelText := Clipboard.AsText;
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

Procedure TMainForm.DoInputValid(Sender: TObject);
Begin
  With MainGlobe Do
    Begin
      { If there is an output coordinate system selected, perform the conversion. }
      If OutputPanel.CoordinateSystemSelected Then
        DoOutputChangeSystem(Self);
      // TODO: Need to perform conversion here for non-geodetic coordinates.
      Marker.Lat := InputPanel.Coordinates.Latitude;
      Marker.Lon := InputPanel.Coordinates.Longitude;
      ShowMarker := True;
      Refresh;
    End;
End;

Procedure TMainForm.DoOutputChangeSystem(Sender: TObject);
Begin
  If InputPanel.Valid Then
    Begin
      // TODO: Perform conversion.
      OutputPanel.Coordinates := InputPanel.Coordinates;
    End;
End;

Procedure TMainForm.SetupDataGrid;
Var
  Col, LastCol: Integer;
  NewWidth, AlternativeWidth: Integer;
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

