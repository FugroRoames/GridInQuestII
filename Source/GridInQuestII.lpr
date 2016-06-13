Program GridInQuestII;

{ GridInQuest II Coordinate Transformation Utility Program.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
  for more details. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

{ $DEFINE LOGDEBUG} { Enable define to generate log file for heap trace. }

Uses
  {$IFDEF UNIX}
    cthreads,
    cmem,
  {$ENDIF}
  {$IFDEF LOGDEBUG}
    SysUtils,
  {$ENDIF}
  Interfaces, Splash, Forms, Controls, Main, ETRS, BNG, ITM, IG, OSGeoid, Config;

{$R *.res}

Begin
  {$IFDEF LOGDEBUG}SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.log'));{$ENDIF}
  Application.Title:='Grid InQuest II';
  RequireDerivedFormResource := True;
  Screen.Cursor := crHourGlass;
  Application.Initialize;
  ShowSplashForm(2000);
  Application.CreateForm(TMainForm, MainForm);
  Screen.Cursor := crDefault;
  Application.Run;
End.
