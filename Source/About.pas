Unit About;

{ GridInQuest II Coordinate Transformation Utility About Unit.

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
  TAboutForm = Class(TForm)
    VersionLabel: TLabel;
    CopyrightLabel: TLabel;
    MemoryLabel: TLabel;
    LicenceLabel: TLabel;
    TrademarksLabel: TLabel;
    LPSImage: TImage;
    OSImage: TImage;
    OSiImage: TImage;
    IconPanel: TPanel;
    InformationPageControl: TPageControl;
    InformationTabSheet: TTabSheet;
    Trademarks: TTabSheet;
    LicenceTabSheet: TTabSheet;
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Procedure ShowAboutForm();

Implementation

{$R *.lfm}

Procedure ShowAboutForm();
Var
  MessageText: String;
  VersionInfo: TFileVersionInfo;
  {$IFDEF Windows}
  MemoryStatus: TMemoryStatus;
  {$ENDIF}
  //HeapStatus: TFPCHeapStatus;
Begin
  With TAboutForm.Create(Application.MainForm) Do
    Begin
      VersionInfo := TFileVersionInfo.Create(Nil);
      Try
        VersionInfo.FileName := Application.ExeName;
        VersionInfo.ReadFileInfo;
        With VersionInfo.VersionStrings Do
          MessageText := Values['ProductName']+' '+Values['FileDescription']+LineEnding+
                         Values['LegalCopyright']+LineEnding+Values['Comments'];
        CopyrightLabel.Caption := MessageText;
        With VersionInfo.VersionStrings Do
          MessageText := 'Version: '+Values['FileVersion']+'  Date: '+{$I %DATE%}+'  Time: '+{$I %TIME%}+LineEnding+
                         'Built with Lazarus '+lcl_version+' and Free Pascal '+{$I %FPCVERSION%};
        VersionLabel.Caption := MessageText;
      Finally
        VersionInfo.Free;
      End;
      MessageText := EmptyStr;
      {$IFDEF Windows}
      MemoryStatus.dwLength := SizeOf(MemoryStatus);
      GlobalMemoryStatus(MemoryStatus);
      MessageText := 'RAM Installed: '+IntToStr(MemoryStatus.dwTotalPhys Div 1024)+'Kb '+ //HeapStatus.CurrHeapSize
                     'RAM Available: '+IntToStr(MemoryStatus.dwAvailPhys Div 1024)+'Kb '+LineEnding;  //HeapStatus.CurrHeapFree
      {$ENDIF}
{ TODO: Get this working!
      HeapStatus := GetFPCHeapStatus;
      MessageText := MessageText+
                     'Current RAM Used: '+IntToStr(HeapStatus.CurrHeapUsed Div 1024)+'Kb '+
                     'Highest RAM Used: '+IntToStr(HeapStatus.MaxHeapUsed Div 1024)+'Kb';
      MemoryLabel.Caption := MessageText;    }
      ShowModal;
      Free;
    End;
End;

End.

