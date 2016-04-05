Unit CtrlUtils;

{ Cross Platform Control Handling Utilities.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
  for more details. }

{ Usage Note:
  Geodesy must follow Geometry on the Uses clause for the redefinition of
  TCoordinates to be visible in code. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, StdCtrls;

Procedure CheckComboBoxWidth(ThisComboBox: TComboBox; LimitWidth: Integer = 0);
Procedure CheckButtonSize(ThisButton: TButton);

Implementation

Const
  ComboButtonWidth = 40;
  MinButtonWidth = 100;

Procedure CheckComboBoxWidth(ThisComboBox: TComboBox; LimitWidth: Integer = 0);
Var
  Index, LastIndex: Integer;
  CurrentWidth, MaximumWidth: Integer;
Begin
  MaximumWidth := ThisComboBox.Width;
  LastIndex := ThisComboBox.Items.Count-1;
  For Index := 0 To LastIndex Do
    Begin
      CurrentWidth := ComboButtonWidth+ThisComboBox.Canvas.TextWidth(ThisComboBox.Items[Index]);
      If CurrentWidth>MaximumWidth Then
        If (CurrentWidth>LimitWidth) And (LimitWidth<>0) Then
          MaximumWidth := LimitWidth
        Else
          MaximumWidth := CurrentWidth;
    End;
  If MaximumWidth>ThisComboBox.Width Then
    ThisComboBox.Width := MaximumWidth;
End;

Procedure CheckButtonSize(ThisButton: TButton);
Var
  PreferredWidth, PreferredHeight: Integer;
Begin
  ThisButton.GetPreferredSize(PreferredWidth, PreferredHeight);
  ThisButton.Height := PreferredHeight;
  If PreferredWidth<MinButtonWidth Then
    PreferredWidth := MinButtonWidth;
  ThisButton.Width := PreferredWidth;
End;

End.

