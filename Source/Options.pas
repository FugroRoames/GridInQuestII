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
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

Type
  TOptionsForm = Class(TForm)
    BottomPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
  Private
    { private declarations }
  Public
    { public declarations }
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

End.

