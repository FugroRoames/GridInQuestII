Unit About;

{ GridInQuest II Coordinate Transformation Utility About Unit.

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

Interface

Uses
  Classes, SysUtils, FileInfo, FileUtil, LCLVersion, {$IFDEF Windows}Windows, {$ENDIF}
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

Type
  TAboutForm = Class(TForm)
    CommentsLabel: TLabel;
    NameLabel: TLabel;
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
  ImageWidth, TextWidth: Integer;
Begin
  With TAboutForm.Create(Application.MainForm) Do
    Begin
      VersionInfo := TFileVersionInfo.Create(Nil);
      Try
        VersionInfo.FileName := Application.ExeName;
        VersionInfo.ReadFileInfo;
        With VersionInfo.VersionStrings Do
          MessageText := Values['ProductName']+' '+Values['FileDescription'];
        NameLabel.Caption := MessageText;
        With VersionInfo.VersionStrings Do
          MessageText := Values['LegalCopyright'];
        CopyrightLabel.Caption := MessageText;
        With VersionInfo.VersionStrings Do
          MessageText := Values['Comments'];
        CommentsLabel.Caption := MessageText;
        With VersionInfo.VersionStrings Do
          MessageText := 'Version: '+Values['FileVersion']+'  Date: '+{$I %DATE%}+'  Time: '+{$I %TIME%}+LineEnding+
                         'Built with Lazarus '+lcl_version+' and Free Pascal '+{$I %FPCVERSION%};
        VersionLabel.Caption := MessageText;
      Finally
        VersionInfo.Free;
      End;
      {$IFDEF Windows}
      MemoryStatus.dwLength := SizeOf(MemoryStatus);
      GlobalMemoryStatus(MemoryStatus);
      MessageText := 'RAM Installed: '+IntToStr(MemoryStatus.dwTotalPhys Div 1024)+'Kb '+
                     'RAM Available: '+IntToStr(MemoryStatus.dwAvailPhys Div 1024)+'Kb '+LineEnding;
      {$ELSE}
      MessageText := EmptyStr;
      {$ENDIF}
      MemoryLabel.Caption := MessageText;
      {$IFDEF Windows}
      NameLabel.Font.Name := 'Arial';
      CopyrightLabel.Font.Name := 'Arial';
      CommentsLabel.Font.Name := 'Arial';
      VersionLabel.Font.Name := 'Arial';
      MemoryLabel.Font.Name := 'Arial';
      TrademarksLabel.Font.Name := 'Arial';
      LicenceLabel.Font.Name := 'Arial';
      NameLabel.Font.Size := 14;
      CopyrightLabel.Font.Size := 12;
      CommentsLabel.Font.Size := 12;
      VersionLabel.Font.Size := 12;
      MemoryLabel.Font.Size := 12;
      TrademarksLabel.Font.Size := 12;
      LicenceLabel.Font.Size := 12;
      {$ELSE}
        {$IFDEF Darwin}
        NameLabel.Font.Size := 18;
        CopyrightLabel.Font.Size := 16;
        CommentsLabel.Font.Size := 16;
        VersionLabel.Font.Size := 16;
        MemoryLabel.Font.Size := 16;
        TrademarksLabel.Font.Size := 16;
        LicenceLabel.Font.Size := 16;
        {$ELSE}
        NameLabel.Font.Size := 12;
        CopyrightLabel.Font.Size := 10;
        CommentsLabel.Font.Size := 10;
        VersionLabel.Font.Size := 10;
        MemoryLabel.Font.Size := 10;
        TrademarksLabel.Font.Size := 10;
        LicenceLabel.Font.Size := 10;
        {$ENDIF}
      {$ENDIF}
      ImageWidth := LPSImage.Picture.Width+OSImage.Picture.Width+OSiImage.Picture.Width+60;
      TextWidth := NameLabel.Canvas.TextWidth(NameLabel.Caption)+60;
      If ImageWidth>TextWidth Then
        Width := ImageWidth
      Else
        Width := TextWidth;
      ShowModal;
      Free;
    End;
End;

End.

