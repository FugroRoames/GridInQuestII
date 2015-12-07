Unit Options;

{ GridInQuest II Coordinate Transformation Utility Options Unit.

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
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, //LMessages,
  Config, OSTab, Geodesy, IG, ITM;

Type

  { TOptionsForm }

  TOptionsForm = Class(TForm)
    BottomPanel: TPanel;
    CancelButton: TButton;
    CartesianDisplayGroupBox: TGroupBox;
    CartesianDisplayGroupBox1: TGroupBox;
    CartesianPlacesEdit: TEdit;
    CartesianPlacesEdit1: TEdit;
    CartesianPlacesLabel: TLabel;
    CartesianPlacesLabel1: TLabel;
    CartesianStyleComboBox: TComboBox;
    CartesianStyleComboBox1: TComboBox;
    CartesianStyleLabel: TLabel;
    CartesianStyleLabel1: TLabel;
    DatumSuffixCheckBox: TCheckBox;
    DatumSuffixCheckBox1: TCheckBox;
    GeodeticDisplayGroupBox: TGroupBox;
    GeodeticDisplayGroupBox1: TGroupBox;
    GeodeticPlacesEdit: TEdit;
    GeodeticPlacesEdit1: TEdit;
    GeodeticPlacesLabel: TLabel;
    GeodeticPlacesLabel1: TLabel;
    GeodeticStyleComboBox: TComboBox;
    GeodeticStyleComboBox1: TComboBox;
    GeodeticStyleLabel: TLabel;
    GeodeticStyleLabel1: TLabel;
    GeodeticUnitsComboBox: TComboBox;
    GeodeticUnitsComboBox1: TComboBox;
    GeodeticUnitsLabel: TLabel;
    GeodeticUnitsLabel1: TLabel;
    HeightDisplayGroupBox: TGroupBox;
    HeightDisplayGroupBox1: TGroupBox;
    HeightPlacesEdit: TEdit;
    HeightPlacesEdit1: TEdit;
    HeightPlacesLabel: TLabel;
    HeightPlacesLabel1: TLabel;
    HeightStyleComboBox: TComboBox;
    HeightStyleComboBox1: TComboBox;
    HeightStyleLabel: TLabel;
    HeightStyleLabel1: TLabel;
    LongitudeCheckBox: TCheckBox;
    LongitudeCheckBox1: TCheckBox;
    OKButton: TButton;
    OptionsPageControl: TPageControl;
    IrishSettingsGroupBox: TGroupBox;
    IGDatumComboBox: TComboBox;
    ITMDatumComboBox: TComboBox;
    IGDatumLabel: TLabel;
    ITMDatumLabel: TLabel;
    ProjectedDisplayGroupBox: TGroupBox;
    ProjectedDisplayGroupBox1: TGroupBox;
    ProjectedPlacesEdit: TEdit;
    ProjectedPlacesEdit1: TEdit;
    ProjectedPlacesLabel: TLabel;
    ProjectedPlacesLabel1: TLabel;
    ProjectedStyleComboBox: TComboBox;
    ProjectedStyleComboBox1: TComboBox;
    ProjectedStyleLabel: TLabel;
    ProjectedStyleLabel1: TLabel;
    QuadrantsComboBox: TComboBox;
    QuadrantsComboBox1: TComboBox;
    QuadrantsLabel: TLabel;
    QuadrantsLabel1: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    WhitespaceCheckBox: TCheckBox;
    WhitespaceCheckBox1: TCheckBox;
    Procedure FormShow(Sender: TObject);
    Procedure IrishSettingsGroupBoxResize(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
    procedure OptionsPageControlResize(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Procedure ShowOptionsForm();

Implementation

{$R *.lfm}

Const
  ComboButtonWidth = 40;

Procedure ShowOptionsForm();
Begin
  With TOptionsForm.Create(Application.MainForm) Do
    Begin
      ShowModal;
      Free;
    End;
End;

Procedure TOptionsForm.FormShow(Sender: TObject);
Begin
  If IGCoordinateSystem.PreferredVerticalDatum=vdMalinHead Then
    IGDatumComboBox.Text := 'Malin Head'
  Else
    IGDatumComboBox.Text := 'Belfast';
  If ITMCoordinateSystem.PreferredVerticalDatum=vdMalinHead Then
    ITMDatumComboBox.Text := 'Malin Head'
  Else
    ITMDatumComboBox.Text := 'Belfast';
End;

Procedure TOptionsForm.IrishSettingsGroupBoxResize(Sender: TObject);
Begin
  { Set the widths to match the maximum needed for the ComboBoxes. }
  IGDatumComboBox.Width := ComboButtonWidth+IGDatumComboBox.Canvas.TextWidth('Malin Head');
  ITMDatumComboBox.Width := ComboButtonWidth+ITMDatumComboBox.Canvas.TextWidth('Malin Head');
End;

Procedure TOptionsForm.OKButtonClick(Sender: TObject);
Begin
  Case IGDatumComboBox.Text Of
  'Belfast':
    IGCoordinateSystem.PreferredVerticalDatum := vdBelfast;
  'Malin Head':
    IGCoordinateSystem.PreferredVerticalDatum := vdMalinHead;
  End;
  Case ITMDatumComboBox.Text Of
  'Belfast':
    ITMCoordinateSystem.PreferredVerticalDatum := vdBelfast;
  'Malin Head':
    ITMCoordinateSystem.PreferredVerticalDatum := vdMalinHead;
  End;
  WriteConfigOptions;
End;

Procedure TOptionsForm.OptionsPageControlResize(Sender: TObject);
Begin
  OptionsPageControl.ClientHeight := GeodeticDisplayGroupBox.Height+
                                     ProjectedDisplayGroupBox.Height+
                                     CartesianDisplayGroupBox.Height+
                                     HeightDisplayGroupBox.Height+25;
End;

End.

