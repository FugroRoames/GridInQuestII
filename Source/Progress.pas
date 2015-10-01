Unit Progress;

{ GridInQuest II Coordinate Transformation Utility Progress Dialog Unit.

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
  {$MODE ObjFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, LResources, LCLType, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls,  Menus, StdCtrls;

Type
  TProgressForm = Class(TForm)
    CaptionLabel: TLabel;
    ProgressPanel: TPanel;
    BorderShape: TShape;
    ProgressBar: TProgressBar;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
  End;

Procedure CreateProgressForm;
Procedure ShowProgressForm(Progress: Integer; NewCaption: String = '');
Procedure HideProgressForm;

Implementation

Var
  ProgressForm: TProgressForm;

Procedure CreateProgressForm;
Begin
  If Not Assigned(ProgressForm) Then
    ProgressForm := TProgressForm.Create(Nil);
End;

Procedure ShowProgressForm(Progress: Integer; NewCaption: String = '');
Begin
  If Assigned(ProgressForm) Then
    With ProgressForm Do
      Begin
        CaptionLabel.Caption := NewCaption;
        If Showing Then
          BringToFront
        Else
          Begin
            Application.MainForm.Update;
            ShowOnTop;
          End;
        With ProgressBar Do
          Begin
            Screen.Cursor := crAppStart;
            Position := Progress;
            If Position=Max Then
              If Showing Then
                Begin
                  Close;
                  Application.MainForm.Update;
                End;
          End;
        If Showing Then
          Begin
            Update;
            Sleep(5);
          End;
      End;
End;

Procedure HideProgressForm;
Begin
  If Assigned(ProgressForm) Then
    Begin
      ProgressForm.Close;
      If Assigned(Application.MainForm) Then
        Application.MainForm.BringToFront;
    End;
End;

Procedure TProgressForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  Screen.Cursor := crDefault;
  CloseAction := caHide;
End;

Initialization

{$I Progress.lrs}

ProgressForm := Nil;

Finalization

FreeAndNil(ProgressForm);

End.

