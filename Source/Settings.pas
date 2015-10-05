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
  StdCtrls, ExtCtrls, DataStreams, Geodesy;

Type
  TSettingsForm = Class(TForm)
    BottomPanel: TPanel;
    TextDelimiterCheckBox: TCheckBox;
    VirticalDataCheckBox: TCheckBox;
    HeaderRowCheckBox: TCheckBox;
    EndRowEdit: TEdit;
    ErrorHandlingComboBox: TComboBox;
    ErrorHandlingLabel: TLabel;
    HeaderRowEdit: TEdit;
    EndRowLabel: TLabel;
    HeaderRowLabel: TLabel;
    FirstFieldLabel: TLabel;
    OutputSystemComboBox: TComboBox;
    OutputSystemLabel: TLabel;
    SecondFieldLabel: TLabel;
    StartRowEdit: TEdit;
    StartRowLabel: TLabel;
    ThirdFieldLabel: TLabel;
    TextDelimiterComboBox: TComboBox;
    TextDelimiterLabel: TLabel;
    FixedColumnBreaksEdit: TEdit;
    FixedColumnBreaksLabel: TLabel;
    ColumnDelimiterComboBox: TComboBox;
    ColumnDelimiterLabel: TLabel;
    FirstColumnComboBox: TComboBox;
    InputFileGroupBox: TGroupBox;
    FileFormatLabel: TLabel;
    InputSystemComboBox: TComboBox;
    OuputDataGroupBox: TGroupBox;
    InputDataGroupBox: TGroupBox;
    OKButton: TButton;
    CancelButton: TButton;
    SecondColumnComboBox: TComboBox;
    FileFormatComboBox: TComboBox;
    InputSystemLabel: TLabel;
    ThirdColumnComboBox: TComboBox;
    Procedure ColumnDelimiterComboBoxChange(Sender: TObject);
    Procedure EndRowEditEditingDone(Sender: TObject);
    Procedure FileFormatComboBoxChange(Sender: TObject);
    Procedure FirstColumnComboBoxChange(Sender: TObject);
    Procedure FixedColumnBreaksEditEditingDone(Sender: TObject);
    Procedure HeaderRowCheckBoxChange(Sender: TObject);
    Procedure HeaderRowEditEditingDone(Sender: TObject);
    Procedure InputSystemComboBoxChange(Sender: TObject);
    Procedure OutputSystemComboBoxChange(Sender: TObject);
    Procedure SecondColumnComboBoxChange(Sender: TObject);
    Procedure StartRowEditEditingDone(Sender: TObject);
    Procedure TextDelimiterCheckBoxChange(Sender: TObject);
    Procedure TextDelimiterComboBoxChange(Sender: TObject);
    Procedure ThirdColumnComboBoxChange(Sender: TObject);
    Procedure VirticalDataCheckBoxChange(Sender: TObject);
  Private
    { Private declarations. }
    Data: TDataStream;
    Procedure DisplayDataInformation;
  End;

Function ShowSettingsForm(NewData: TDataStream): Boolean;

Implementation

{$R *.lfm}

Uses
  Main;

Function ShowSettingsForm(NewData: TDataStream): Boolean;
Begin
  With TSettingsForm.Create(Application.MainForm) Do
    Begin
      Data := NewData;
      DisplayDataInformation;
      Result := (ShowModal=mrOK);
      Free;
    End;
End;

Procedure TSettingsForm.DisplayDataInformation;
Var
  Index, LastIndex: Integer;
  BreaksListText: String;
Begin
  Case Data.FormatType Of
  ftDelimited:
    Begin
      FileFormatComboBox.ItemIndex := 0;
      ColumnDelimiterComboBox.Enabled := True;
      FixedColumnBreaksEdit.Enabled := False;
    End;
  ftFixed:
    Begin
      FileFormatComboBox.ItemIndex := 1;
      ColumnDelimiterComboBox.Enabled := False;
      FixedColumnBreaksEdit.Enabled := True;
    End;
  End;
  Case Data.FieldTerminator Of
  ',':
    ColumnDelimiterComboBox.ItemIndex := 0;
  ';':
    ColumnDelimiterComboBox.ItemIndex := 1;
  #9:
    ColumnDelimiterComboBox.ItemIndex := 2;
  Else { Custom }
    ColumnDelimiterComboBox.ItemIndex := 3;
  End;
  If FixedColumnBreaksEdit.Enabled Then
    Begin
      BreaksListText := EmptyStr;
      LastIndex := Data.FieldCount-1;
      For Index := 0 To LastIndex Do
        Begin
          BreaksListText := BreaksListText+IntToStr(Data.FieldStarts[Index]);
          If Index<LastIndex Then
            BreaksListText := BreaksListText+', ';
        End;
      FixedColumnBreaksEdit.Text := BreaksListText;
    End
  Else
    FixedColumnBreaksEdit.Text := '';
  TextDelimiterCheckBox.Checked := (Data.TextDelimiter<>#0);
  TextDelimiterComboBox.Enabled := (Data.TextDelimiter<>#0);
  Case Data.TextDelimiter Of
  #0:
    TextDelimiterComboBox.ItemIndex := -1;
  '''':
    TextDelimiterComboBox.ItemIndex := 0;
  '"':
    TextDelimiterComboBox.ItemIndex := 1;
  Else
    TextDelimiterComboBox.ItemIndex := 2;
  End;
  HeaderRowCheckBox.Checked := (Data.NameRow<>-1);
  HeaderRowEdit.Enabled := (Data.NameRow<>-1);
  If Data.NameRow=-1 Then
    HeaderRowEdit.Text := EmptyStr
  Else
    HeaderRowEdit.Text := IntToStr(Data.NameRow+1);
  InputSystemComboBox.Items.Text := CoordinateSystems.AvailableSystemsList();
  StartRowEdit.Text := IntToStr(Data.FirstRow+1);
  If Data.LastRow=-1 Then
    EndRowEdit.Text := EmptyStr
  Else
    EndRowEdit.Text := IntToStr(Data.LastRow+1);
  FirstColumnComboBox.Items.Text := Data.NamesList;
  FirstColumnComboBox.ItemIndex := MainForm.InputLatIndex;
  SecondColumnComboBox.Items.Text := Data.NamesList;
  SecondColumnComboBox.ItemIndex := MainForm.InputLonIndex;
  ThirdColumnComboBox.Items.Text := Data.NamesList;
  ThirdColumnComboBox.ItemIndex := MainForm.InputAltIndex;
  OutputSystemComboBox.Items.Text := CoordinateSystems.AvailableSystemsList();
End;

Procedure TSettingsForm.FileFormatComboBoxChange(Sender: TObject);
Begin
  Case FileFormatComboBox.ItemIndex Of
  0:
    Data.FormatType := ftDelimited;
  1:
    Data.FormatType := ftFixed;
  End;
  DisplayDataInformation;
End;

Procedure TSettingsForm.ColumnDelimiterComboBoxChange(Sender: TObject);
Begin
  Case ColumnDelimiterComboBox.ItemIndex Of
  0: { Comma }
    Data.FieldTerminator := ',';
  1: { Semicolon }
    Data.FieldTerminator := ';';
  2: { Tab }
    Data.FieldTerminator := #9;
  3: { Custom }
    Data.FieldTerminator := #0; //TODO?
  End;
  DisplayDataInformation;
End;

Procedure TSettingsForm.FixedColumnBreaksEditEditingDone(Sender: TObject);
Begin
  // TODO: Parse column breaks.
  DisplayDataInformation;
End;

Procedure TSettingsForm.TextDelimiterCheckBoxChange(Sender: TObject);
Begin
  TextDelimiterComboBox.Enabled := TextDelimiterCheckBox.Checked;
  If TextDelimiterComboBox.Enabled Then
    Data.TextDelimiter := '"'
  Else
    Data.TextDelimiter := #0;
  DisplayDataInformation;
End;

Procedure TSettingsForm.TextDelimiterComboBoxChange(Sender: TObject);
Begin
  Case TextDelimiterComboBox.ItemIndex Of
  0: { Comma }
    Data.TextDelimiter := '''';
  1: { Semicolon }
    Data.TextDelimiter := '"';
  2: { Custom }
    Data.TextDelimiter := #0; //TODO?
  End;
  DisplayDataInformation;
End;

Procedure TSettingsForm.HeaderRowCheckBoxChange(Sender: TObject);
Begin
  HeaderRowEdit.Enabled := HeaderRowCheckBox.Checked;
  If HeaderRowEdit.Enabled Then
    Data.NameRow := 0
  Else
    Data.NameRow := -1;
  DisplayDataInformation;
End;

Procedure TSettingsForm.HeaderRowEditEditingDone(Sender: TObject);
Begin
  Data.NameRow := StrToIntDef(HeaderRowEdit.Text, 1)-1;
  DisplayDataInformation;
End;

Procedure TSettingsForm.StartRowEditEditingDone(Sender: TObject);
Begin
  Data.FirstRow := StrToIntDef(StartRowEdit.Text, 1)-1;
  DisplayDataInformation;
End;

Procedure TSettingsForm.EndRowEditEditingDone(Sender: TObject);
Begin
  Data.LastRow := StrToIntDef(EndRowEdit.Text, 0)-1;
  DisplayDataInformation;
End;

Procedure TSettingsForm.InputSystemComboBoxChange(Sender: TObject);
Begin
  // TODO: Record output coordinate system.
  DisplayDataInformation;
End;

Procedure TSettingsForm.FirstColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputLatIndex := FirstColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

Procedure TSettingsForm.SecondColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputLonIndex := SecondColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

Procedure TSettingsForm.VirticalDataCheckBoxChange(Sender: TObject);
Begin
  ThirdColumnComboBox.Enabled := VirticalDataCheckBox.Checked;
  If Not ThirdColumnComboBox.Enabled Then
    Begin
      MainForm.InputAltIndex := -1;
      DisplayDataInformation;
    End;
End;

Procedure TSettingsForm.ThirdColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputAltIndex := ThirdColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

Procedure TSettingsForm.OutputSystemComboBoxChange(Sender: TObject);
Begin
  // TODO: Record output coordinate system.
  DisplayDataInformation;
End;

End.

