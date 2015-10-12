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
    Procedure Format(Tidy: Boolean = False);
    Procedure Validate;
  Protected
    { Protected declarations. }
    Procedure DoExit; Override;
    Procedure DoOnResize; Override;
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; NewCaption: String; AxisType: TAxisType; Locked: Boolean); Virtual;
    Procedure Clear;
    Property AxisType: TAxisType Read FAxisType Write FAxisType;
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
    Procedure Clear;
  End;

Type
  TCoordinatesEntryPanel = Class(TPanel)
  Private
    { Private declarations. }
    FCoordinateSystemPanel: TCoordinateSystemPanel;
    FFirstCoordinatePanel: TCoordinatePanel;
    FLocked: Boolean;
    FOnChangeSystem: TNotifyEvent;
    FOnValid: TNotifyEvent;
    FSecondCoordinatePanel: TCoordinatePanel;
    FThirdCoordinatePanel: TCoordinatePanel;
    FPanelType: TPanelType;
    FCoordinateType: TCoordinateType;
    Function GetCoordinates: TCoordinates;
    Function GetCoordinatesAsText: String;
    Function GetValid: Boolean;
    Procedure DoCoordinateValid(Sender: TObject);
    Procedure SetCoordinates(Value: TCoordinates);
    Procedure SetLocked(Value: Boolean);
  Public
    { Public declarations. }
    Constructor Create(TheOwner: TComponent; PanelType: TPanelType); Virtual;
    Procedure Clear;
    Function SelectedCoordinateSystemIndex: Integer;
    Property Coordinates: TCoordinates Read GetCoordinates Write SetCoordinates;
    Property CoordinatesAsText: String Read GetCoordinatesAsText;
    Property CoordinateType: TCoordinateType Read FCoordinateType Write FCoordinateType;
    Property Locked: Boolean Read FLocked Write SetLocked;
    Property Valid: Boolean Read GetValid;
    Property OnChangeSystem: TNotifyEvent Read FOnChangeSystem Write FOnChangeSystem;
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

Procedure TCoordinatePanel.Format(Tidy: Boolean = False);
Begin
  If FEdit.ReadOnly Then
    FEdit.Font.Color := clBlue
  Else
    If Valid Or (FEdit.Text=EmptyStr) Then
      FEdit.Font.Color := clBlack
    Else
      FEdit.Font.Color := clRed;
  If Tidy Then
    Case TCoordinatesEntryPanel(Parent).CoordinateType Of
    ctGeocentric:
      Case AxisType Of
      atXAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm', 2);
      atYAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm', 2);
      atZAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm', 2);
      End;
    ctGeodetic:
      Case AxisType Of
      atXAxis: FEdit.Text := FormatCoordinate(DecimalToSexagesimalCoordinate(Coordinate), soEastWestSuffix);
      atYAxis: FEdit.Text := FormatCoordinate(DecimalToSexagesimalCoordinate(Coordinate), soNorthSouthSuffix);
      atZAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm', 2);
      End;
    ctCartesian:
      Case AxisType Of
      atXAxis: FEdit.Text := FormatCoordinate(Coordinate, 3, True)+' E';
      atYAxis: FEdit.Text := FormatCoordinate(Coordinate, 3, True)+' N';
      atZAxis: FEdit.Text := FormatCoordinateWithUnits(Coordinate, 'm', 2);
      End;
    End;
End;


Procedure TCoordinatePanel.Validate;
Begin
  Case TCoordinatesEntryPanel(Parent).CoordinateType Of
  ctGeocentric:
    FValid := TryGeocentricTextToCoordinate(FEdit.Text, FCoordinate, FAxisType);
  ctGeodetic:
    FValid := TryGeodeticTextToCoordinate(FEdit.Text, FCoordinate, FAxisType);
  ctCartesian:
    FValid := TryCartesianTextToCoordinate(FEdit.Text, FCoordinate, FAxisType);
  Else
    FValid := False;
  End;
  Format;
End;

Procedure TCoordinatePanel.DoExit;
Begin
  Format(True);
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
      Items.Text := CoordinateSystems.AvailableSystemsList;
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

Procedure TCoordinateSystemPanel.Clear;
Begin
  FComboBox.ItemIndex := -1;
End;

Procedure TCoordinatesEntryPanel.SetLocked(Value: Boolean);
Begin
  If FLocked<>Value Then
    Begin
      FLocked := Value;
      FFirstCoordinatePanel.FEdit.ReadOnly := Value;
      FSecondCoordinatePanel.FEdit.ReadOnly := Value;
      FThirdCoordinatePanel.FEdit.ReadOnly := Value;
      FFirstCoordinatePanel.FEdit.TabStop := Not Value;
      FSecondCoordinatePanel.FEdit.TabStop := Not Value;
      FThirdCoordinatePanel.FEdit.TabStop := Not Value;
    End;
End;

Procedure TCoordinateSystemPanel.DoOnChange(Sender: TObject);
Var
  Index: Integer;
  CoordinateSystem: TCoordinateSystem;
  Coordinates: TCoordinates;
  Procedure SetAxisCaptionAndType(CoordinatePanel: TCoordinatePanel; Index: Integer; AxisOrder: TAxisOrder; AxisNames: TAxisNames);
  Begin
    CoordinatePanel.AxisType := AxisTypeFromIndex(Index, AxisOrder);
    Case CoordinatePanel.AxisType Of
    atXAxis: CoordinatePanel.FLabel.Caption := AxisNames.LongX+':';
    atYAxis: CoordinatePanel.FLabel.Caption := AxisNames.LongY+':';
    atZAxis: CoordinatePanel.FLabel.Caption := AxisNames.LongZ+':';
    End;
  End;
Begin
  Index := FComboBox.ItemIndex;
  CoordinateSystem := CoordinateSystems.Items(Index);
  TCoordinatesEntryPanel(Parent).Clear;
  TCoordinatesEntryPanel(Parent).Locked := (Index=-1) Or (TCoordinatesEntryPanel(Parent).FPanelType=ptOutput);
  FComboBox.ItemIndex := Index;
  With TCoordinatesEntryPanel(Parent) Do
    Begin
      CoordinateType := CoordinateSystem.CoordinateType;
      With CoordinateSystem Do
        Begin
          SetAxisCaptionAndType(FFirstCoordinatePanel, 0, AxisOrder, AxisNames);
          SetAxisCaptionAndType(FSecondCoordinatePanel, 1, AxisOrder, AxisNames);
          SetAxisCaptionAndType(FThirdCoordinatePanel, 2, AxisOrder, AxisNames);
        End;
    End;
  With TCoordinatesEntryPanel(Parent) Do
    If Assigned(OnChangeSystem) Then
      OnChangeSystem(Self);
End;

Procedure TCoordinateSystemPanel.DoOnResize;
Begin
  Inherited DoOnResize;
  FLabel.Width := Self.ClientWidth Div 4;
End;

Constructor TCoordinatesEntryPanel.Create(TheOwner: TComponent; PanelType: TPanelType);
Var
  ThisPanel: TPanel;
  NewCaption: String;
Begin
  Inherited Create(TheOwner);
  Align := alTop;
  AutoSize := True;
  ThisPanel := Self;
  { The child controls are created in reverse order so that Align=alTop orders them correctly. }
  FThirdCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Z coordinate:', atZAxis, True);
  FThirdCoordinatePanel.Parent := ThisPanel;
  FThirdCoordinatePanel.OnValid := @DoCoordinateValid;
  FSecondCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'Y Coordinate:', atYAxis, True);
  FSecondCoordinatePanel.Parent := ThisPanel;
  FSecondCoordinatePanel.OnValid := @DoCoordinateValid;
  FFirstCoordinatePanel := TCoordinatePanel.Create(TheOwner, 'X Coordinate:', atXAxis, True);
  FFirstCoordinatePanel.Parent := ThisPanel;
  FFirstCoordinatePanel.OnValid := @DoCoordinateValid;
  Locked := True;
  FPanelType := PanelType;
  FCoordinateType := ctGeodetic;
  Case PanelType Of
  ptInput: NewCaption := 'Input System:';
  ptOutput: NewCaption := 'Output System:';
  End;
  FCoordinateSystemPanel := TCoordinateSystemPanel.Create(TheOwner, NewCaption);
  FCoordinateSystemPanel.Parent := ThisPanel;
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

Procedure TCoordinatesEntryPanel.SetCoordinates(Value: TCoordinates);
  Procedure SetCoordinateForAxis(Coordinates: TCoordinates; Edit: TEdit; AxisType: TAxisType);
  Begin
    Case AxisType Of
    atXAxis: Edit.Text := FormatCoordinate(Coordinates.X);
    atYAxis: Edit.Text := FormatCoordinate(Coordinates.Y);
    atZAxis: Edit.Text := FormatCoordinate(Coordinates.Z);
    End;
  End;
Begin
  With FFirstCoordinatePanel Do
    Begin
      SetCoordinateForAxis(Value, FEdit, AxisType);
      Format(True);
    End;
  With FSecondCoordinatePanel Do
    Begin
      SetCoordinateForAxis(Value, FEdit, AxisType);
      Format(True);
    End;
  With FThirdCoordinatePanel Do
    Begin
      SetCoordinateForAxis(Value, FEdit, AxisType);
      Format(True);
    End;
End;

Procedure TCoordinatesEntryPanel.Clear;
Begin
  FCoordinateSystemPanel.Clear;
  FFirstCoordinatePanel.Clear;
  FSecondCoordinatePanel.Clear;
  FThirdCoordinatePanel.Clear;
End;

Function TCoordinatesEntryPanel.SelectedCoordinateSystemIndex: Integer;
Begin
  Result := CoordinateSystems.FindByDescription(FCoordinateSystemPanel.FComboBox.Text);
End;

Procedure Register;
Begin
  RegisterComponents('Additional', [TCoordinatePanel]);
End;

End.
