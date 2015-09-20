Unit Transform;

{ GridInQuest II Coordinate Transformation Utility Transform Settings Unit.

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
  Classes, SysUtils, FileInfo, FileUtil, LCLVersion, {$IFDEF Windows}Windows, {$ENDIF}
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

Type
  TTransformForm = Class(TForm)
    BottomPanel: TPanel;
    Button1: TButton;
    ErrorHandlingComboBox: TComboBox;
    ErrorHandlingLabel: TLabel;
    FirstColumnComboBox: TComboBox;
    HeaderRowCheckBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    SecondColumnComboBox: TComboBox;
    StartRowEdit: TEdit;
    StartRowLabel: TLabel;
    ThirdColumnComboBox: TComboBox;
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Function ShowTransformForm(): Boolean;

Implementation

{$R *.lfm}

Function ShowTransformForm(): Boolean;
Begin
  With TTransformForm.Create(Application.MainForm) Do
    Begin
      Result := (ShowModal=mrOK);
      Free;
    End;
End;

(*
InputCSVData: TCSVData;

SetupDataDisplay;
Procedure HeaderRowCheckBoxChange(Sender: TObject);
Procedure StartRowEditChange(Sender: TObject);

Procedure TMainForm.StartRowEditChange(Sender: TObject);
Begin
  SetupDataDisplay;
End;

Procedure TMainForm.HeaderRowCheckBoxChange(Sender: TObject);
Begin
  SetupDataDisplay;
End;

Procedure TMainForm.SetupDataDisplay;
Var
  Col, LastCol: Integer;
  NewWidth, AlternativeWidth: Integer;
Begin
  If HeaderRowCheckBox.Checked Then
    InputCSVData.NameRow := 0
  Else
    InputCSVData.NameRow := -1;
  InputCSVData.StartRow := StrToIntDef(StartRowEdit.Text, 1)-1;
  FirstColumnComboBox.Items.Text := InputCSVData.NamesList;
  SecondColumnComboBox.Items.Text := InputCSVData.NamesList;
  ThirdColumnComboBox.Items.Text := InputCSVData.NamesList;
  PointsDrawGrid.Columns.Clear;
  PointsDrawGrid.RowCount := InputCSVData.RowCount+1;
  PointsDrawGrid.FixedRows := 1;
  PointsDrawGrid.FixedCols := 1;
  Canvas.Font := PointsDrawGrid.Font;
  { Ensure the fixed column is wide enough to fit two more than the number of digets required for the row count. }
  NewWidth := Canvas.TextWidth(StringOfChar('0', 2+Length(IntToStr(InputCSVData.RowCount))));
  PointsDrawGrid.ColWidths[0] := NewWidth;
  LastCol := InputCSVData.ColCount-1;
  For Col := 0 To LastCol Do
    With PointsDrawGrid.Columns.Add Do
      Begin
        Title.Caption := InputCSVData.Names[Col];
        { Calculate the width of the caption plus a couple of spaces. }
        NewWidth := Canvas.TextWidth('  '+Title.Caption);
        { Calculate the width of the first data item plus a couple of spaces. }
        AlternativeWidth := Canvas.TextWidth('  '+InputCSVData.Values[0, Col]);
        { Choose the wider of the two widths. }
        If AlternativeWidth>NewWidth Then
          Width := AlternativeWidth
        Else
          Width := NewWidth;
      End;
End;


*)
End.

