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
  ExtCtrls,  Menus;

Type
  TProgressForm = Class(TForm)
    CloseMenuItem: TMenuItem;
    ClosePopupMenu: TPopupMenu;
    ProgressBar: TProgressBar;
    Procedure FormCreate(Sender: TObject);
    Procedure CloseMenuItemClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
  Private
    Activated: Boolean;
  End;

Procedure InitializeProgressForm;
Procedure SetProgressActivation(Activate: Boolean);
Procedure ShowProgress(Progress: Integer; Caption: String = '');
Procedure ShowProgressForm(Caption: String = '');
Procedure HideProgressForm;

Implementation

Var
  ProgressForm: TProgressForm;

Procedure InitializeProgressForm;
Begin
  Application.CreateForm(TProgressForm, ProgressForm);
End;

Procedure SetProgressActivation(Activate: Boolean);
Begin
  Try
    ProgressForm.Activated := Activate;
  Except
    LogErrorMessage('Progress Form Enable Error');
  End;
End;

Procedure ShowProgress(Progress: Integer; Caption: String = '');
Begin
  Try
    If ProgressForm=Nil Then
      InitializeProgressForm;
    If ProgressForm.Activated Then
      Begin
        ShowProgressForm(Caption);
        With ProgressForm.ProgressBar Do
          Begin
            If Position<>Progress Then
              StepBy(Progress-Position);
            If Position=Max Then
              Begin
                Application.ProcessMessages;
                ProgressForm.Close;
              End;
          End;
      End;
  Except
    // TODO: Close on error
  End;
End;

Procedure ShowProgressForm(Caption: String = '');
Begin
  Screen.Cursor := crAppStart;
  Try
    If ProgressForm=Nil Then
      InitializeProgressForm;
    If ProgressForm.Activated Then
      Begin
        If Caption<>EmptyStr Then
          Begin
            ProgressForm.Caption := Caption;
            ProgressForm.Update;
          End;
        If Not ProgressForm.Showing Then
          ProgressForm.Show;
        ProgressForm.BringToFront;
      End;
  Except
    // TODO: Close on error
  End;
End;

Procedure HideProgressForm;
Begin
  Screen.Cursor := crDefault;
  If ProgressForm<>Nil Then
    Try
      ProgressForm.Close;
      If Assigned(Application.MainForm) Then
        Application.MainForm.BringToFront;
    Except
      // TODO: Close on error
    End;
End;

Procedure TProgressForm.FormCreate(Sender: TObject);
Begin
//  {$IFDEF Windows}
//  If Assigned(Application.MainForm) Then
//    SetWindowLong(Self.Handle,GWL_HWNDPARENT,Application.MainForm.Handle);
//  {$ENDIF}
  ProgressBar.DoubleBuffered := True;
  Activated := False;
End;

Procedure TProgressForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  CloseAction := caHide;
End;

Procedure TProgressForm.CloseMenuItemClick(Sender: TObject);
Begin
  Close;
End;

Initialization

{$I Progress.lrs}

End.

