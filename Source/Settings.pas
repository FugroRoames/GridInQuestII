Unit Settings;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, DataStreams;

Type
  TSettingsForm = Class(TForm)
    BottomPanel: TPanel;
    Button1: TButton;
    ErrorHandlingComboBox: TComboBox;
    InputSystemComboBox: TComboBox;
    ErrorHandlingLabel: TLabel;
    FirstColumnComboBox: TComboBox;
    HeaderRowCheckBox: TCheckBox;
    InputSystemComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    Panel1: TPanel;
    SecondColumnComboBox: TComboBox;
    StartRowEdit: TEdit;
    StartRowLabel: TLabel;
    ThirdColumnComboBox: TComboBox;
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Function ShowSettingsForm(Data: TDataStream): Boolean;

Implementation

{$R *.lfm}

Function ShowSettingsForm(Data: TDataStream): Boolean;
Begin
  With TSettingsForm.Create(Application.MainForm) Do
    Begin
      FirstColumnComboBox.Items.Text := Data.NamesList;
      SecondColumnComboBox.Items.Text := Data.NamesList;
      ThirdColumnComboBox.Items.Text := Data.NamesList;
      Result := (ShowModal=mrOK);
      Free;
    End;
End;

(*

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
End;


*)
End.

