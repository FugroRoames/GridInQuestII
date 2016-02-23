Unit Splash;

{ Splash Screen Creation Unit.

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
  Classes, SysUtils, Forms, Graphics, Controls, StdCtrls, ExtCtrls, FileInfo;

Procedure ShowSplashForm(DisplayInterval: Integer = 4000);

Implementation

{ This unit requires a bitmap resource containing the splash screen image. }
{TODO: Remove the need for Mac define when OSX resource compiler working. }
{$IFDEF Darwin}
  {$R Splashscreen.res}
{$ELSE}
  {$R Splashscreen.rc}
{$ENDIF}

Type
  TSplashThread = Class(TThread)
  Private
    SplashForm: TForm;
    Procedure CreateSplashForm;
    Procedure FreeSplashForm;
  Protected
    Procedure Execute; Override;
  Public
    Delay: Integer;
    Constructor Create;
  End;

Procedure ShowSplashForm(DisplayInterval: Integer = 4000);
Var
  SplashThread: TSplashThread;
Begin
  SplashThread := TSplashThread.Create;
  SplashThread.Delay := DisplayInterval;
  SplashThread.Start;
End;

Constructor TSplashThread.Create;
Begin
  Priority := tpLowest;
  FreeOnTerminate := True;
  Inherited Create(True);
End;

Procedure TSplashThread.Execute;
Begin
  Synchronize(@CreateSplashForm);
  Sleep(Delay);
  Synchronize(@FreeSplashForm);
End;

Procedure TSplashThread.CreateSplashForm;
Var
  SplashscreenStream: TResourceStream;
  SplashscreenJpeg: TJPEGImage;
  VersionInfo: TFileVersionInfo;
Begin
  SplashscreenStream := TResourceStream.Create(hInstance, 'SPLASHSCREEN', 'JPEG');
  Try
    SplashscreenJpeg := TJPEGImage.Create;
    SplashscreenJpeg.LoadFromStream(SplashscreenStream);
    VersionInfo := TFileVersionInfo.Create(Nil);
    Try
      SplashForm := TForm.Create(Nil);
      With SplashForm Do
        Begin
          AutoSize := True;
          FormStyle := fsStayOnTop;
          With TImage.Create(SplashForm) Do
            Begin
              BorderStyle := bsNone;
              Picture.Bitmap.Assign(SplashscreenJpeg);
              Parent := SplashForm;
              SetBounds(0, 0, Picture.Width, Picture.Height);
            End;
          With TLabel.Create(SplashForm) Do
            Begin
              Align := alBottom;
              Alignment := taRightJustify;
              BorderSpacing.Right := 20;
              BorderSpacing.Bottom := 18;
              Font.Color := clWhite;
              Font.Size := 9;
              Font.Style:= [fsBold];
              Font.Quality := fqCleartypeNatural;
              Parent := SplashForm;
              With VersionInfo Do
                Begin
                  FileName := Application.ExeName;
                  Enabled := True;
                  Caption := 'Version: '+VersionStrings.Values['ProductVersion'];
                End;
            End;
          Position := poScreenCenter;
          Show;
        End;
      Application.ProcessMessages;
    Except
    End;
  Finally
    SplashscreenJpeg.Free;
    SplashscreenStream.Free;
    VersionInfo.Free;
  End;
End;

Procedure TSplashThread.FreeSplashForm;
Begin
  FreeAndNil(SplashForm);
End;

End.

