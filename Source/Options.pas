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
  Config, OSTab, Geodesy, IG, ITM, GeodUtils;

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
    CompactCheckBox: TCheckBox;
    CompactOutputCheckBox: TCheckBox;
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
  Function OptionsToText(Options: TTypedOptions): String;
  Begin
    If toSexagseimalSymbols In Options Then
      Result := 'Symbols'
    Else If toSexagseimalLetters In Options Then
      Result := 'Letters'
    Else
      Result := 'None'
  End;
  Function DatumCodeToText(Code: TVerticalDatumCode): String;
  Begin
    Case Code Of
    vdBelfast:
      Result := 'Belfast';
    vdMalinHead:
      Result := 'Malin Head';
    End;
  End;
Begin
  With InteractiveSettings Do
    Begin
      //GeodeticStyleComboBox.Text := (GeodeticStyle);
      GeodeticUnitsComboBox.Text := OptionsToText(GeodeticUnits);
      QuadrantsComboBox.Text := GeodeticQuadrants;
      GeodeticPlacesEdit.Text := IntToStr(GeodeticDecimalPlaces);
      CompactCheckBox.Checked := GeodeticCompactFormat;
      LongitudeCheckBox.Checked := GeodeticPositiveLongitude;
      //ProjectedStyleComboBox.Text := (ProjectedStyle);
      ProjectedPlacesEdit.Text := IntToStr(ProjectedDecimalPlaces);
      //CartesianStyleComboBox.Text := (CartesianStyle);
      CartesianPlacesEdit.Text := IntToStr(CartesianDecimalPlaces);
      //HeightStyleComboBox.Text := (HeightStyle);
      HeightPlacesEdit.Text := IntToStr(HeightDecimalPlaces);
      DatumSuffixCheckBox.Checked := HeightDatumSuffix;
    End;
  IGDatumComboBox.Text := DatumCodeToText(IGCoordinateSystem.PreferredVerticalDatum);
  ITMDatumComboBox.Text := DatumCodeToText(ITMCoordinateSystem.PreferredVerticalDatum);
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
  Function TextToUnits(Text: String): TTypedOptions;
  Begin
    Case Text Of
    'Symbols': Result := [toSexagseimalSymbols];
    'Letters': Result := [toSexagseimalLetters];
    Else
      Result := [];
    End;
  End;
  Function DatumTextToCode(Text: String): TVerticalDatumCode;
  Begin
    If SameText(IGDatumComboBox.Text, 'Belfast') Then
      Result := vdBelfast
    Else
      Result := vdMalinHead;
  End;
Begin
  With InteractiveSettings Do
    Begin
      //GeodeticStyle := GetValue(GeodeticStyleComboBox.Text);
      GeodeticUnits := TextToUnits(GeodeticUnitsComboBox.Text);
      GeodeticQuadrants := QuadrantsComboBox.Text;
      GeodeticDecimalPlaces := StrToIntDef(GeodeticPlacesEdit.Text, 2);
      GeodeticCompactFormat := CompactCheckBox.Checked;
      GeodeticPositiveLongitude := LongitudeCheckBox.Checked;
      //ProjectedStyle := GetValue(ProjectedStyleComboBox.Text);
      ProjectedDecimalPlaces := StrToIntDef(ProjectedPlacesEdit.Text, 2);
      //CartesianStyle := GetValue(CartesianStyleComboBox.Text);
      CartesianDecimalPlaces := StrToIntDef(CartesianPlacesEdit.Text, 2);
      //HeightStyle := GetValue(HeightStyleComboBox.Text);
      HeightDecimalPlaces := StrToIntDef(HeightPlacesEdit.Text, 2);
      HeightDatumSuffix := DatumSuffixCheckBox.Checked;
    End;
  IGCoordinateSystem.PreferredVerticalDatum := DatumTextToCode(IGDatumComboBox.Text);
  ITMCoordinateSystem.PreferredVerticalDatum := DatumTextToCode(ITMDatumComboBox.Text);
  WriteConfigOptions;
End;

End.

