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
  StdCtrls, ExtCtrls, DOM, XMLConf, CtrlUtils, DataStreams, Geodesy;

Type
  TSettingsForm = Class(TForm)
    BottomPanel: TPanel;
    LoadSettingsButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveSettingsButton: TButton;
    TextDelimiterCheckBox: TCheckBox;
    ConsecutiveDelimitersCheckBox: TCheckBox;
    VerticalDataCheckBox: TCheckBox;
    HeaderRowCheckBox: TCheckBox;
    EndRowEdit: TEdit;
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
    Procedure ConsecutiveDelimitersCheckBoxChange(Sender: TObject);
    Procedure EndRowEditEditingDone(Sender: TObject);
    Procedure FileFormatComboBoxChange(Sender: TObject);
    Procedure FirstColumnComboBoxChange(Sender: TObject);
    Procedure FixedColumnBreaksEditEditingDone(Sender: TObject);
    Procedure FixedColumnBreaksEditKeyPress(Sender: TObject; Var Key: char);
    Procedure HeaderRowCheckBoxChange(Sender: TObject);
    Procedure HeaderRowEditEditingDone(Sender: TObject);
    Procedure InputDataGroupBoxResize(Sender: TObject);
    Procedure InputFileGroupBoxResize(Sender: TObject);
    Procedure InputSystemComboBoxChange(Sender: TObject);
    procedure LoadSettingsButtonClick(Sender: TObject);
    Procedure OutputSystemComboBoxChange(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    Procedure SecondColumnComboBoxChange(Sender: TObject);
    Procedure StartRowEditEditingDone(Sender: TObject);
    Procedure TextDelimiterCheckBoxChange(Sender: TObject);
    Procedure TextDelimiterComboBoxChange(Sender: TObject);
    Procedure ThirdColumnComboBoxChange(Sender: TObject);
    Procedure VerticalDataCheckBoxChange(Sender: TObject);
  Private
    Data: TDataStream;
    OldFormatType: TFormatType;
    OldFieldTerminator: Char;
    OldConsecutiveDelimiters: Boolean;
    OldBreaksList: String;
    OldTextDelimiter: Char;
    OldNameRow: Integer;
    OldFirstRow: Integer;
    OldLastRow: Integer;
    OldInputSystemIndex: Integer;
    OldFirstFieldIndex: Integer;
    OldSecondFieldIndex: Integer;
    OldThirdFieldIndex: Integer;
    OldOutputSystemIndex: Integer;
    Procedure CacheSettings;
    Procedure DisplayDataInformation;
    Procedure LoadSettings(LoadFileName: String);
    Procedure ParseBreaksList(BreaksText: String);
    Procedure RestoreSettings;
    Procedure SaveSettings(SaveFileName: String);
  End;

Function ShowSettingsForm(NewData: TDataStream): Boolean;

Implementation

{$R *.lfm}

Uses
  Main;

Const
  InputKey = 'InputSettings';
  OutputKey = 'OutputSettings';
  EPSGNumberKey = 'EPSGNumber';
  FormatKey = 'Format';
  DelimitedKey = 'Delimited';
  FixedKey = 'Fixed';

Function ShowSettingsForm(NewData: TDataStream): Boolean;
Begin
  With TSettingsForm.Create(Application.MainForm) Do
    Begin
      Data := NewData;
      DisplayDataInformation;
      CacheSettings;
      Result := (ShowModal=mrOK);
      { Clear any prior transform on ok or restore original settings if cancel is pressed. }
      If Result Then
        With MainForm Do
          Begin
            SetLength(OutputCoordinates, 0);
            SetupDataGrid;
          End
      Else
        RestoreSettings;
      Free;
    End;
End;

procedure TSettingsForm.DisplayDataInformation;
Var
  Index, LastIndex: Integer;
  BreaksListText: String;
  Procedure SetAxisCaption(FieldLabel: TLabel; Index: Integer; AxisOrder: TAxisOrder; AxisNames: TAxisNames);
  Begin
    Case AxisTypeFromIndex(Index, AxisOrder) Of
    atXAxis: FieldLabel.Caption := AxisNames.LongX+':';
    atYAxis: FieldLabel.Caption := AxisNames.LongY+':';
    atZAxis: FieldLabel.Caption := AxisNames.LongZ+':';
    End;
  End;
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
  ColumnDelimiterComboBox.Items[3] := 'Custom';
  Case Data.FieldTerminator Of
  ',':
    ColumnDelimiterComboBox.ItemIndex := 0;
  ';':
    ColumnDelimiterComboBox.ItemIndex := 1;
  #9:
    ColumnDelimiterComboBox.ItemIndex := 2;
  Else { Custom }
    ColumnDelimiterComboBox.ItemIndex := 3;
    ColumnDelimiterComboBox.Items[3] := 'Custom: '+Data.FieldTerminator;
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
  TextDelimiterComboBox.Items[2] := 'Custom';
  Case Data.TextDelimiter Of
  #0:
    TextDelimiterComboBox.ItemIndex := -1;
  '''':
    TextDelimiterComboBox.ItemIndex := 0;
  '"':
    TextDelimiterComboBox.ItemIndex := 1;
  Else
    TextDelimiterComboBox.ItemIndex := 2;
    TextDelimiterComboBox.Items[2] := 'Custom: '+Data.TextDelimiter;
  End;
  HeaderRowCheckBox.Checked := (Data.NameRow<>-1);
  HeaderRowEdit.Enabled := (Data.NameRow<>-1);
  If Data.NameRow=-1 Then
    HeaderRowEdit.Text := EmptyStr
  Else
    HeaderRowEdit.Text := IntToStr(Data.NameRow+1);
  InputSystemComboBox.Items.Text := CoordinateSystems.AvailableSystemsList();
  If MainForm.InputSystemIndex=-1 Then
    InputSystemComboBox.Text := EmptyStr
  Else
    With CoordinateSystems.Items(MainForm.InputSystemIndex) Do
      Begin
        InputSystemComboBox.Text := Description;
        SetAxisCaption(FirstFieldLabel, 0, AxisOrder, AxisNames);
        SetAxisCaption(SecondFieldLabel, 1, AxisOrder, AxisNames);
        SetAxisCaption(ThirdFieldLabel, 2, AxisOrder, AxisNames);
        If CoordinateType=ctCartesian Then
          Begin
            VerticalDataCheckBox.Checked := True;
            VerticalDataCheckBox.Enabled := False;
          End
        Else
          Begin
            VerticalDataCheckBox.Checked := (MainForm.InputThirdFieldIndex<>-1);
            VerticalDataCheckBox.Enabled := True;
          End;
      End;
  StartRowEdit.Text := IntToStr(Data.FirstRow+1);
  If Data.LastRow=-1 Then
    EndRowEdit.Text := EmptyStr
  Else
    EndRowEdit.Text := IntToStr(Data.LastRow+1);
  FirstColumnComboBox.Items.Text := Data.NamesList;
  FirstColumnComboBox.ItemIndex := MainForm.InputFirstFieldIndex;
  SecondColumnComboBox.Items.Text := Data.NamesList;
  SecondColumnComboBox.ItemIndex := MainForm.InputSecondFieldIndex;
  ThirdColumnComboBox.Items.Text := Data.NamesList;
  ThirdColumnComboBox.ItemIndex := MainForm.InputThirdFieldIndex;
  OutputSystemComboBox.Items.Text := CoordinateSystems.CompatibleSystemsList(InputSystemComboBox.ItemIndex);
  If (MainForm.OutputSystemIndex=-1) Or (MainForm.OutputSystemIndex=MainForm.InputSystemIndex) Then
    OutputSystemComboBox.Text := EmptyStr
  Else
    With CoordinateSystems.Items(MainForm.OutputSystemIndex) Do
      OutputSystemComboBox.Text := Description;
  MainForm.SetupDataGrid;
End;

procedure TSettingsForm.CacheSettings;
Begin
  OldFormatType := Data.FormatType;
  OldFieldTerminator := Data.FieldTerminator;
  OldConsecutiveDelimiters := Data.ConsecutiveDelimiters;
  OldBreaksList := FixedColumnBreaksEdit.Text;
  OldTextDelimiter := Data.TextDelimiter;
  OldNameRow := Data.NameRow;
  OldFirstRow := Data.FirstRow;
  OldLastRow := Data.LastRow;
  OldInputSystemIndex := MainForm.InputSystemIndex;
  OldFirstFieldIndex := MainForm.InputFirstFieldIndex;
  OldSecondFieldIndex := MainForm.InputSecondFieldIndex;
  OldThirdFieldIndex := MainForm.InputThirdFieldIndex;
  OldOutputSystemIndex := MainForm.OutputSystemIndex;
End;

Procedure TSettingsForm.LoadSettings(LoadFileName: String);
Var
  XMLSettings: TXMLConfig;
Begin
  XMLSettings := TXMLConfig.Create(Nil);
  With XMLSettings Do
    Begin
      { Open settings configuration file. }
      RootName := 'Settings';
      Filename := LoadFileName;
      { Read input settings. }
      OpenKey(InputKey);
      Case GetValue(FormatKey, DelimitedKey) Of
      DelimitedKey: Data.FormatType := ftDelimited;
      FixedKey: Data.FormatType := ftFixed;
      End;
      //Data.FieldTerminator := OldFieldTerminator;
      //Data.ConsecutiveDelimiters := OldConsecutiveDelimiters;
      //ParseBreaksList(OldBreaksList);
      //Data.TextDelimiter := OldTextDelimiter;
      //Data.NameRow := OldNameRow;
      //Data.FirstRow := OldFirstRow;
      //Data.LastRow := OldLastRow;
      MainForm.InputSystemIndex := CoordinateSystems.FindEPSGNumber(GetValue(EPSGNumberKey, 0));
      //MainForm.InputFirstFieldIndex := OldFirstFieldIndex;
      //MainForm.InputSecondFieldIndex := OldSecondFieldIndex;
      //MainForm.InputThirdFieldIndex := OldThirdFieldIndex;
      CloseKey;
      { Read output settings. }
      OpenKey(OutputKey);
      MainForm.OutputSystemIndex := CoordinateSystems.FindEPSGNumber(GetValue(EPSGNumberKey, 0));
      CloseKey;
      { Close settings configuration file. }
      Free;
    End;
  DisplayDataInformation;
End;

Procedure TSettingsForm.SaveSettings(SaveFileName: String);
Var
  XMLSettings: TXMLConfig;
Begin
  XMLSettings := TXMLConfig.Create(Nil);
  With XMLSettings Do
    Begin
      { Open settings configuration file. }
      RootName := 'Settings';
      Filename := SaveFileName;
      { Write input settings. }
      OpenKey(InputKey);
      Case Data.FormatType Of
      ftDelimited: SetValue(FormatKey, DelimitedKey);
      ftFixed: SetValue(FormatKey, FixedKey);
      End;
      With CoordinateSystems Do
        SetValue(EPSGNumberKey, Items(FindEPSGNumber(MainForm.InputSystemIndex)).EPSGNumber);
      CloseKey;
      { Write output settings. }
      OpenKey(OutputKey);
      With CoordinateSystems Do
        SetValue(EPSGNumberKey, Items(FindEPSGNumber(MainForm.OutputSystemIndex)).EPSGNumber);
      CloseKey;
      { Close settings configuration file. }
      Flush;
      Free;
    End;
End;

procedure TSettingsForm.RestoreSettings;
Begin
  Data.FormatType := OldFormatType;
  Data.FieldTerminator := OldFieldTerminator;
  Data.ConsecutiveDelimiters := OldConsecutiveDelimiters;
  ParseBreaksList(OldBreaksList);
  Data.TextDelimiter := OldTextDelimiter;
  Data.NameRow := OldNameRow;
  Data.FirstRow := OldFirstRow;
  Data.LastRow := OldLastRow;
  MainForm.InputSystemIndex := OldInputSystemIndex;
  MainForm.InputFirstFieldIndex := OldFirstFieldIndex;
  MainForm.InputSecondFieldIndex := OldSecondFieldIndex;
  MainForm.InputThirdFieldIndex := OldThirdFieldIndex;
  MainForm.OutputSystemIndex := OldOutputSystemIndex;
  MainForm.SetupDataGrid;
End;

procedure TSettingsForm.ParseBreaksList(BreaksText: String);
Var
  BreaksList: TStringList;
  Index, LastIndex: Integer;
Begin
  If Data.FormatType=ftFixed Then
    Begin
      BreaksList := TStringList.Create;
      BreaksList.CommaText := BreaksText;
      Data.FieldCount := BreaksList.Count;
      LastIndex := Data.FieldCount-1;
      For Index := 0 To LastIndex Do
        Begin
          Data.FieldStarts[Index] := StrToInt(BreaksList[Index]);
          If Index>0 Then
            Data.FieldLengths[Index-1] := 1+Data.FieldStarts[Index]-Data.FieldStarts[Index-1];
        End;
      Data.FieldLengths[LastIndex] := 1+Data.RowLength(Data.FirstRow)-Data.FieldStarts[LastIndex];
      BreaksList.Free;
    End;
End;

procedure TSettingsForm.FileFormatComboBoxChange(Sender: TObject);
Begin
  Case FileFormatComboBox.ItemIndex Of
  0:
    Data.FormatType := ftDelimited;
  1:
    Data.FormatType := ftFixed;
  End;
  DisplayDataInformation;
End;

procedure TSettingsForm.ColumnDelimiterComboBoxChange(Sender: TObject);
Var
  InputText: String;
Begin
  Case ColumnDelimiterComboBox.ItemIndex Of
  0: { Comma }
    Data.FieldTerminator := ',';
  1: { Semicolon }
    Data.FieldTerminator := ';';
  2: { Tab }
    Data.FieldTerminator := #9;
  3: { Custom }
    Begin
      InputText := InputBox('Input Settings', 'Enter Custom Terminator Character:', Data.FieldTerminator);
      If InputText=EmptyStr Then
        Data.FieldTerminator := #0
      Else
        Data.FieldTerminator := InputText[1];
    End;
  End;
  DisplayDataInformation;
End;

procedure TSettingsForm.ConsecutiveDelimitersCheckBoxChange(Sender: TObject);
Begin
  Data.ConsecutiveDelimiters := ConsecutiveDelimitersCheckBox.Checked;
  DisplayDataInformation;
End;

procedure TSettingsForm.FixedColumnBreaksEditEditingDone(Sender: TObject);
Begin
  ParseBreaksList(FixedColumnBreaksEdit.Text);
  DisplayDataInformation;
End;

procedure TSettingsForm.FixedColumnBreaksEditKeyPress(Sender: TObject;
  var Key: char);
Begin
  { Only preserve backspace, numbers, spaces and commas. }
  If Not (Key In [#8, '0'..'9',' ',',']) Then
    Key := #0;
End;

procedure TSettingsForm.TextDelimiterCheckBoxChange(Sender: TObject);
Begin
  TextDelimiterComboBox.Enabled := TextDelimiterCheckBox.Checked;
  If TextDelimiterComboBox.Enabled Then
    Data.TextDelimiter := '"'
  Else
    Data.TextDelimiter := #0;
  DisplayDataInformation;
End;

procedure TSettingsForm.TextDelimiterComboBoxChange(Sender: TObject);
Var
  InputText: String;
Begin
  Case TextDelimiterComboBox.ItemIndex Of
  0: { Comma }
    Data.TextDelimiter := '''';
  1: { Semicolon }
    Data.TextDelimiter := '"';
  2: { Custom }
    Begin
      InputText := InputBox('Input Settings', 'Enter Custom Terminator Character:', Data.TextDelimiter);
      If InputText=EmptyStr Then
        Data.TextDelimiter := #0
      Else
        Data.TextDelimiter := InputText[1];
    End;
  End;
  DisplayDataInformation;
End;

procedure TSettingsForm.HeaderRowCheckBoxChange(Sender: TObject);
Begin
  HeaderRowEdit.Enabled := HeaderRowCheckBox.Checked;
  If HeaderRowEdit.Enabled Then
    Data.NameRow := 0
  Else
    Data.NameRow := -1;
  DisplayDataInformation;
End;

procedure TSettingsForm.HeaderRowEditEditingDone(Sender: TObject);
Begin
  Data.NameRow := StrToIntDef(HeaderRowEdit.Text, 1)-1;
  DisplayDataInformation;
End;

procedure TSettingsForm.InputFileGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(FileFormatComboBox);
  CheckComboBoxWidth(ColumnDelimiterComboBox);
  CheckComboBoxWidth(TextDelimiterComboBox);
End;

procedure TSettingsForm.InputDataGroupBoxResize(Sender: TObject);
Var
  MaxWidth: Integer;
Begin
  MaxWidth := InputSystemComboBox.Width Div 3;
  CheckComboBoxWidth(FirstColumnComboBox, MaxWidth);
  CheckComboBoxWidth(SecondColumnComboBox, MaxWidth);
  CheckComboBoxWidth(ThirdColumnComboBox, MaxWidth);
End;

procedure TSettingsForm.StartRowEditEditingDone(Sender: TObject);
Begin
  Data.FirstRow := StrToIntDef(StartRowEdit.Text, 1)-1;
  DisplayDataInformation;
End;

procedure TSettingsForm.EndRowEditEditingDone(Sender: TObject);
Begin
  Data.LastRow := StrToIntDef(EndRowEdit.Text, 0)-1;
  DisplayDataInformation;
End;

procedure TSettingsForm.InputSystemComboBoxChange(Sender: TObject);
Begin
  MainForm.InputSystemIndex := CoordinateSystems.FindByDescription(InputSystemComboBox.Text);
  DisplayDataInformation;
End;

procedure TSettingsForm.LoadSettingsButtonClick(Sender: TObject);
Begin
  With OpenDialog Do
    If Execute Then
      LoadSettings(FileName);
End;

procedure TSettingsForm.FirstColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputFirstFieldIndex := FirstColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

procedure TSettingsForm.SecondColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputSecondFieldIndex := SecondColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

procedure TSettingsForm.VerticalDataCheckBoxChange(Sender: TObject);
Begin
  ThirdColumnComboBox.Enabled := VerticalDataCheckBox.Checked;
  If Not ThirdColumnComboBox.Enabled Then
    Begin
      MainForm.InputThirdFieldIndex := -1;
      DisplayDataInformation;
    End;
End;

procedure TSettingsForm.ThirdColumnComboBoxChange(Sender: TObject);
Begin
  MainForm.InputThirdFieldIndex := ThirdColumnComboBox.ItemIndex;
  DisplayDataInformation;
End;

procedure TSettingsForm.OutputSystemComboBoxChange(Sender: TObject);
Begin
  MainForm.OutputSystemIndex := CoordinateSystems.FindByDescription(OutputSystemComboBox.Text);
  DisplayDataInformation;
End;

procedure TSettingsForm.SaveSettingsButtonClick(Sender: TObject);
Begin
  With SaveDialog Do
    If Execute Then
      SaveSettings(FileName);
End;

End.

