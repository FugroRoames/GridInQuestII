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
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, CtrlUtils,
  Config, OSTab, Geodesy, IG, ITM;

Type
  TOptionsForm = Class(TForm)
    BottomPanel: TPanel;
    CancelButton: TButton;
    CartesianDisplayGroupBox: TGroupBox;
    CartesianOutputGroupBox: TGroupBox;
    CartesianPlacesEdit: TEdit;
    CartesianPlacesOutputEdit: TEdit;
    CartesianPlacesLabel: TLabel;
    CartesianPlacesOutputLabel: TLabel;
    CartesianStyleComboBox: TComboBox;
    CartesianStyleOutputComboBox: TComboBox;
    CartesianStyleLabel: TLabel;
    CartesianStyleOutputLabel: TLabel;
    DatumSuffixCheckBox: TCheckBox;
    DatumSuffixOutputCheckBox: TCheckBox;
    GeodeticDisplayGroupBox: TGroupBox;
    GeodeticOutputGroupBox: TGroupBox;
    GeodeticPlacesEdit: TEdit;
    GeodeticPlacesOutputEdit: TEdit;
    GeodeticPlacesLabel: TLabel;
    GeodeticPlacesOutputLabel: TLabel;
    GeodeticStyleComboBox: TComboBox;
    GeodeticStyleOutputComboBox: TComboBox;
    GeodeticStyleLabel: TLabel;
    GeodeticStyleOutputLabel: TLabel;
    GeodeticUnitsComboBox: TComboBox;
    GeodeticUnitsOutputComboBox: TComboBox;
    GeodeticUnitsLabel: TLabel;
    GeodeticUnitsOutputLabel: TLabel;
    HeightDisplayGroupBox: TGroupBox;
    HeightOutputGroupBox: TGroupBox;
    HeightPlacesEdit: TEdit;
    HeightPlacesOutputEdit: TEdit;
    HeightPlacesLabel: TLabel;
    HeightPlacesOutputLabel: TLabel;
    HeightStyleComboBox: TComboBox;
    HeightStyleOutputComboBox: TComboBox;
    HeightStyleLabel: TLabel;
    HeightStyleOutputLabel: TLabel;
    LongitudeCheckBox: TCheckBox;
    LongitudeOutputCheckBox: TCheckBox;
    OKButton: TButton;
    OptionsPageControl: TPageControl;
    IrishSettingsGroupBox: TGroupBox;
    IGDatumComboBox: TComboBox;
    ITMDatumComboBox: TComboBox;
    IGDatumLabel: TLabel;
    ITMDatumLabel: TLabel;
    ProjectedDisplayGroupBox: TGroupBox;
    ProjectedOutputGroupBox: TGroupBox;
    ProjectedPlacesEdit: TEdit;
    ProjectedPlacesOutputEdit: TEdit;
    ProjectedPlacesLabel: TLabel;
    ProjectedPlacesOutputLabel: TLabel;
    ProjectedStyleComboBox: TComboBox;
    ProjectedStyleOutputComboBox: TComboBox;
    ProjectedStyleLabel: TLabel;
    ProjectedStyleOutputLabel: TLabel;
    QuadrantsComboBox: TComboBox;
    QuadrantsOutputComboBox: TComboBox;
    QuadrantsLabel: TLabel;
    QuadrantsOutputLabel: TLabel;
    InteractiveTabSheet: TTabSheet;
    OutputTabSheet: TTabSheet;
    WhitespaceCheckBox: TCheckBox;
    WhitespaceOutputCheckBox: TCheckBox;
    Procedure CartesianDisplayGroupBoxResize(Sender: TObject);
    Procedure CartesianOutputGroupBoxResize(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure GeodeticOutputGroupBoxResize(Sender: TObject);
    Procedure HeightDisplayGroupBoxResize(Sender: TObject);
    Procedure HeightOutputGroupBoxResize(Sender: TObject);
    Procedure OptionsPageControlResize(Sender: TObject);
    Procedure GeodeticDisplayGroupBoxResize(Sender: TObject);
    Procedure IrishSettingsGroupBoxResize(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
    Procedure ProjectedDisplayGroupBoxResize(Sender: TObject);
    Procedure ProjectedOutputGroupBoxResize(Sender: TObject);
  End;

Procedure ShowOptionsForm();

Implementation

{$R *.lfm}

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

Procedure TOptionsForm.OptionsPageControlResize(Sender: TObject);
Begin
  OptionsPageControl.ClientHeight := GeodeticDisplayGroupBox.Height+
                                     ProjectedDisplayGroupBox.Height+
                                     CartesianDisplayGroupBox.Height+
                                     HeightDisplayGroupBox.Height+25;
End;

Procedure TOptionsForm.GeodeticDisplayGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(GeodeticStyleComboBox);
  CheckComboBoxWidth(GeodeticUnitsComboBox);
  CheckComboBoxWidth(QuadrantsComboBox);
End;

Procedure TOptionsForm.ProjectedDisplayGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(ProjectedStyleComboBox);
End;

Procedure TOptionsForm.CartesianDisplayGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(CartesianStyleComboBox);
End;

Procedure TOptionsForm.HeightDisplayGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(HeightStyleComboBox);
End;

Procedure TOptionsForm.GeodeticOutputGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(GeodeticStyleOutputComboBox);
  CheckComboBoxWidth(GeodeticUnitsOutputComboBox);
  CheckComboBoxWidth(QuadrantsOutputComboBox);
End;

Procedure TOptionsForm.ProjectedOutputGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(ProjectedStyleOutputComboBox);
End;

Procedure TOptionsForm.CartesianOutputGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(CartesianStyleOutputComboBox);
End;

Procedure TOptionsForm.HeightOutputGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(HeightStyleOutputComboBox);
End;

Procedure TOptionsForm.IrishSettingsGroupBoxResize(Sender: TObject);
Begin
  CheckComboBoxWidth(IGDatumComboBox);
  CheckComboBoxWidth(ITMDatumComboBox);
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

End.

