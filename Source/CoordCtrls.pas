Unit CoordCtrls;

{ Coordinate Entry Controls.

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
  {$MODE objfpc}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, LCLType, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Geometry, Geodesy, CoordUtils;

Type
  TPanelType = (ptInput, ptOutput);

Type
  TCoordinatePanel = Class(TPanel)
  Private
    { Private declarations. }
    FAxisType: TAxisType;
    FCoordinateValue: TCoordinate;
    FEdit: TEdit;
    FLabel: TLabel;
    FValid: Boolean;
    Procedure DoOnChange(Sender: TObject);
    Procedure DoOnUTF8KeyPress(Sender: TObject; Var UTF8Key: TUTF8Char);
    Procedure Validate;
  Protected
    { Protected declarations. }
    Procedure DoOnResize; Override;
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; NewCaption: String; AxisType: TAxisType; Locked: Boolean); Virtual;
    Procedure Clear;
    Property CoordinateValue: TCoordinate Read FCoordinateValue;
    Property Valid: Boolean Read FValid;
  End;

Type
  TCoordinateSystemPanel = Class(TPanel)
  Private
    { Private declarations. }
    FComboBox: TComboBox;
    FLabel: TLabel;
  Protected
    { Protected declarations. }
    Procedure DoOnResize; Override;
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; NewCaption: String); Virtual;
    Procedure Clear(CoordinateSystem: Integer = -1);
  End;

Type
  TCoordinatesEntryPanel = Class(TPanel)
  Private
    { Private declarations. }
    FCoordinateSystemPanel: TCoordinateSystemPanel;
    FFirstCoordinatePanel: TCoordinatePanel;
    FSecondCoordinatePanel: TCoordinatePanel;
    FThirdCoordinatePanel: TCoordinatePanel;
    FPanelType: TPanelType;
    Function GetCoordinates: TCoordinates;
    function GetCoordinatesAsText: String;
    Function GetCoordinatesValid: Boolean;
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; PanelType: TPanelType; CoordinateSystem: Integer = -1); Virtual;
    Procedure Clear(CoordinateSystem: Integer = -1);
    Property Coordinates: TCoordinates Read GetCoordinates;
    Property CoordinatesAsText: String Read GetCoordinatesAsText;
    Property CoordinatesValid: Boolean Read GetCoordinatesValid;
  Published
    { Published declarations. }
  End;

Const
  BorderSize: Integer = 5;

Procedure Register;

Implementation

Constructor TCoordinatePanel.Create(TheOwner: TComponent; NewCaption: String; AxisType: TAxisType; Locked: Boolean);
Var
  ThisPanel: TPanel;
Begin
  FAxisType := AxisType;
  Inherited Create(TheOwner);
  Align := alTop;
  AutoSize := True;
  BorderSpacing.Around := BorderSize;
  BevelOuter := bvNone;
  ThisPanel := Self;
  FEdit := TEdit.Create(TheOwner);
  With FEdit Do
    Begin
      Align := alClient;
      Parent := ThisPanel;
      OnChange := @DoOnChange;
      OnUTF8KeyPress := @DoOnUTF8KeyPress;
      ReadOnly := Locked;
      TabStop := Not Locked;
    End;
    FLabel := TLabel.Create(TheOwner);
  With FLabel Do
    Begin
      Align := alLeft;
      Alignment := taRightJustify;
      AutoSize := False;
      BorderSpacing.Right := BorderSize;
      Caption := NewCaption;
      Layout := tlCenter;
      Parent := ThisPanel;
    End;
  If TheOwner Is TWinControl Then
    Parent := TWinControl(TheOwner);
  Validate;
End;

Procedure TCoordinatePanel.Clear;
Begin
  FCoordinateValue := 0;
  FEdit.Clear;
  FValid := False;
End;

Procedure TCoordinatePanel.Validate;
Begin
  FValid := TryGeodeticTextToCoordinate(FEdit.Text, FCoordinateValue, FAxisType);
  If FEdit.ReadOnly Then
    FEdit.Font.Color := clBlue
  Else
    If FValid Or (FEdit.Text=EmptyStr) Then
      Begin
        FEdit.Font.Color := clBlack;
      End
    Else
      Begin
        FEdit.Font.Color := clRed;
      End;
End;

Procedure TCoordinatePanel.DoOnChange(Sender: TObject);
Begin
  Validate;
//  If Valid Then
// TODO: Fire an on valid event.
End;

Procedure TCoordinatePanel.DoOnUTF8KeyPress(Sender: TObject; Var UTF8Key: TUTF8Char);
Begin
  { Substitute reverse dash for degree symbol. }
  If UTF8Key='`' Then
    UTF8Key := #$C2#$B0; { UTF8 code for '°' character. }
End;

Procedure TCoordinatePanel.DoOnResize;
Begin
  Inherited DoOnResize;
  FLabel.Width := Self.ClientWidth Div 4;
End;

Constructor TCoordinateSystemPanel.Create(TheOwner: TComponent; NewCaption: String);
Var
  ThisPanel: TPanel;
Begin
  Inherited Create(TheOwner);
  Align := alTop;
  AutoSize := True;
  BorderSpacing.Around := BorderSize;
  BevelOuter := bvNone;
  ThisPanel := Self;
  FComboBox := TComboBox.Create(TheOwner);
  With FComboBox Do
    Begin
      Align := alClient;
      Items.Text := GetAvailableSystemsList;
      Parent := ThisPanel;
      Style := csDropDownList;
    End;
  FLabel := TLabel.Create(TheOwner);
  With FLabel Do
    Begin
      Align := alLeft;
      Alignment := taRightJustify;
      AutoSize := False;
      BorderSpacing.Right := BorderSize;
      Caption := NewCaption;
      Layout := tlCenter;
      Parent := ThisPanel;
    End;
  If TheOwner Is TWinControl Then
    Parent := TWinControl(TheOwner);
End;

Procedure TCoordinateSystemPanel.Clear(CoordinateSystem: Integer);
Begin
  FComboBox.ItemIndex := CoordinateSystem;
End;

Procedure TCoordinateSystemPanel.DoOnResize;
Begin
  Inherited DoOnResize;
  FLabel.Width := Self.ClientWidth Div 4;
End;

function TCoordinatesEntryPanel.GetCoordinates: TCoordinates;
Begin
  // TODO: Swap panels depending upon system axis order.
  If CoordinatesValid Then
    Begin
      Result.X := FFirstCoordinatePanel.CoordinateValue;
      Result.Y := FSecondCoordinatePanel.CoordinateValue;
      Result.Z := FThirdCoordinatePanel.CoordinateValue;
    End
  Else
    Result := TCoordinates(NullCoordinates);
End;

Function TCoordinatesEntryPanel.GetCoordinatesAsText: String;
Begin
  Result := 'TODO: Format Coordinates';
End;

function TCoordinatesEntryPanel.GetCoordinatesValid: Boolean;
Begin
  Result := FFirstCoordinatePanel.Valid And
            FSecondCoordinatePanel.Valid And
            FThirdCoordinatePanel.Valid;
End;

constructor TCoordinatesEntryPanel.Create(TheOwner: TComponent;
  PanelType: TPanelType; CoordinateSystem: Integer);
Var
  ThisPanel: TPanel;
  NewCaption: String;
Begin
  Inherited Create(TheOwner);
  Align := alTop;
  AutoSize := True;
  ThisPanel := Self;
  { The child controls are created in reverse order so that Align=alTop orders them correctly. }
  FThirdCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Altitude:', atZAxis, (PanelType=ptOutput));
  FThirdCoordinatePanel.Parent := ThisPanel;
  FSecondCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Longitude:', atXAxis, (PanelType=ptOutput));
  FSecondCoordinatePanel.Parent := ThisPanel;
  FFirstCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Latitude:', atYAxis, (PanelType=ptOutput));
  FFirstCoordinatePanel.Parent := ThisPanel;
  FPanelType := PanelType;
  Case PanelType Of
  ptInput: NewCaption := 'Input System:';
  ptOutput: NewCaption := 'Output System:';
  End;
  FCoordinateSystemPanel := TCoordinateSystemPanel.Create(TheOwner, NewCaption);
  FCoordinateSystemPanel.Parent := ThisPanel;
  FCoordinateSystemPanel.FComboBox.ItemIndex := CoordinateSystem;
  FCoordinateSystemPanel.TabOrder := 0;
  FFirstCoordinatePanel.TabOrder := 1;
  FSecondCoordinatePanel.TabOrder := 2;
  FThirdCoordinatePanel.TabOrder := 3;
  If TheOwner Is TWinControl Then
    Parent := TWinControl(TheOwner);
End;

procedure TCoordinatesEntryPanel.Clear(CoordinateSystem: Integer);
Begin
  FCoordinateSystemPanel.Clear(CoordinateSystem);
  FFirstCoordinatePanel.Clear;
  FSecondCoordinatePanel.Clear;
  FThirdCoordinatePanel.Clear;
End;

Procedure Register;
Begin
  RegisterComponents('Additional', [TCoordinatePanel]);
End;

End.
