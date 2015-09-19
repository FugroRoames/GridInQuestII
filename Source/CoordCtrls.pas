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
  Graphics, Dialogs, StdCtrls, ExtCtrls, Geometry, Geodesy, GeomUtils, GeodUtils;

Type
  TPanelType = (ptInput, ptOutput);

Type
  TCoordinatePanel = Class(TPanel)
  Private
    { Private declarations. }
    FAxisType: TAxisType;
    FCoordinate: TCoordinate;
    FEdit: TEdit;
    FLabel: TLabel;
    FValid: Boolean;
    FOnValid: TNotifyEvent;
    Procedure DoOnChange(Sender: TObject);
    Procedure DoOnUTF8KeyPress(Sender: TObject; Var UTF8Key: TUTF8Char);
    Procedure Validate;
  Protected
    { Protected declarations. }
    Procedure DoExit; Override;
    Procedure DoOnResize; Override;
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; NewCaption: String; AxisType: TAxisType; Locked: Boolean); Virtual;
    Procedure Clear;
    Property AxisType: TAxisType Read FAxisType;
    Property Coordinate: TCoordinate Read FCoordinate;
    Property Valid: Boolean Read FValid;
    Property OnValid: TNotifyEvent Read FOnValid Write FOnValid;
  End;

Type
  TCoordinateSystemPanel = Class(TPanel)
  Private
    { Private declarations. }
    FComboBox: TComboBox;
    FLabel: TLabel;
  Protected
    { Protected declarations. }
    Procedure DoOnChange(Sender: TObject);
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
    FOnValid: TNotifyEvent;
    FSecondCoordinatePanel: TCoordinatePanel;
    FThirdCoordinatePanel: TCoordinatePanel;
    FPanelType: TPanelType;
    Function GetCoordinates: TCoordinates;
    Function GetCoordinatesAsText: String;
    Function GetValid: Boolean;
    Procedure DoCoordinateValid(Sender: TObject);
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; PanelType: TPanelType; CoordinateSystem: Integer = -1); Virtual;
    Procedure Clear(CoordinateSystem: Integer = -1);
    Property Coordinates: TCoordinates Read GetCoordinates;
    Property CoordinatesAsText: String Read GetCoordinatesAsText;
    Property Valid: Boolean Read GetValid;
    Property OnValid: TNotifyEvent Read FOnValid Write FOnValid;
  Published
    { Published declarations. }
  End;

Const
  BorderSize: Integer = 5;

Procedure Register;

Implementation

{$IFDEF Darwin}
Uses
  LazUTF8;
{$ENDIF}

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
  FCoordinate := 0;
  FEdit.Clear;
  FValid := False;
End;

Procedure TCoordinatePanel.Validate;
Begin
  FValid := TryGeodeticTextToCoordinate(FEdit.Text, FCoordinate, FAxisType);
  If FEdit.ReadOnly Then
    FEdit.Font.Color := clBlue
  Else
    If Valid Or (FEdit.Text=EmptyStr) Then
      Begin
        FEdit.Font.Color := clBlack;
      End
    Else
      Begin
        FEdit.Font.Color := clRed;
      End;
End;

Procedure TCoordinatePanel.DoExit;
Begin
  If Valid Then
    Case AxisType Of
    atXAxis: FEdit.Text := FormatCoordinate(DecimalToSexagesimalCoordinate(Coordinate), soEastWestSuffix);
    atYAxis: FEdit.Text := FormatCoordinate(DecimalToSexagesimalCoordinate(Coordinate), soNorthSouthSuffix);
    atZAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm');
    End;
  Inherited DoExit;
End;

Procedure TCoordinatePanel.DoOnChange(Sender: TObject);
Begin
  Validate;
  If Valid Then
    If Assigned(OnValid) Then
      OnValid(Self);
End;

Procedure TCoordinatePanel.DoOnUTF8KeyPress(Sender: TObject; Var UTF8Key: TUTF8Char);
Var
  PartText: String;
Begin
  { Substitute reverse dash for degree symbol. }
  {$IFDEF Darwin}
  If UTF8Key='`' Then
    Begin
      UTF8Key := #0; { remove the reverse dash character. }
      With FEdit Do
        Begin
          PartText := UTF8Copy(Text, 1, SelStart)+'°';
          Text := PartText+UTF8Copy(Text, SelStart+SelLength+1, MaxInt);
          SelStart := UTF8Length(PartText);
        End;
    End;
  {$ELSE}
  If UTF8Key='`' Then
    UTF8Key := #$C2#$B0; { UTF8 code for '°' character. }
  {$ENDIF}
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
      OnChange := @DoOnChange;
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

Procedure TCoordinateSystemPanel.DoOnChange(Sender: TObject);
Begin
  TCoordinatesEntryPanel(Parent).Clear(FComboBox.ItemIndex);
End;

Procedure TCoordinateSystemPanel.DoOnResize;
Begin
  Inherited DoOnResize;
  FLabel.Width := Self.ClientWidth Div 4;
End;

Constructor TCoordinatesEntryPanel.Create(TheOwner: TComponent;
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
  FThirdCoordinatePanel.OnValid := @DoCoordinateValid;
  FSecondCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Longitude:', atXAxis, (PanelType=ptOutput));
  FSecondCoordinatePanel.Parent := ThisPanel;
  FSecondCoordinatePanel.OnValid := @DoCoordinateValid;
  FFirstCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Latitude:', atYAxis, (PanelType=ptOutput));
  FFirstCoordinatePanel.Parent := ThisPanel;
  FFirstCoordinatePanel.OnValid := @DoCoordinateValid;
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

Function TCoordinatesEntryPanel.GetCoordinates: TCoordinates;
  Procedure SetResultAxis(Coordinate: TCoordinate; AxisType: TAxisType);
  Begin
    Case AxisType Of
    atXAxis: Result.X := Coordinate;
    atYAxis: Result.Y := Coordinate;
    atZAxis: Result.Z := Coordinate;
    End;
  End;
Begin
  If Valid Then
    Begin
      With FFirstCoordinatePanel Do
        SetResultAxis(Coordinate, AxisType);
      With FSecondCoordinatePanel Do
        SetResultAxis(Coordinate, AxisType);
      With FThirdCoordinatePanel Do
        SetResultAxis(Coordinate, AxisType);
    End
  Else
    Result := TCoordinates(NullCoordinates);
End;

Function TCoordinatesEntryPanel.GetCoordinatesAsText: String;
Begin
  If Valid Then
    Result := Trim(FFirstCoordinatePanel.FEdit.Text+'  '+
                   FSecondCoordinatePanel.FEdit.Text+'  '+
                   FThirdCoordinatePanel.FEdit.Text)
  Else
    Result := EmptyStr;
End;

Function TCoordinatesEntryPanel.GetValid: Boolean;
Begin
  Result := FFirstCoordinatePanel.Valid And
            FSecondCoordinatePanel.Valid And
            (FThirdCoordinatePanel.Valid Or
            (FThirdCoordinatePanel.FEdit.Text=EmptyStr));
End;

Procedure TCoordinatesEntryPanel.DoCoordinateValid(Sender: TObject);
Begin
  If Valid Then
    If Assigned(OnValid) Then
      OnValid(Self);
End;

Procedure TCoordinatesEntryPanel.Clear(CoordinateSystem: Integer);
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
