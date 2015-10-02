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
  TProgressDisplay = Class
  Private
    FCaption: String;
    FDisplayHeight: Integer;
    FDisplayWidth: Integer;
    FProgress: Integer;
    FProgressForm: TForm;
    FProgressRect: TRect;
    Procedure DoPaint(Sender: TObject);
    Procedure SetProgress(Value: Integer);
    //CaptionLabel: TLabel;
    //ProgressPanel: TPanel;
    //BorderShape: TShape;
    //ProgressBar: TProgressBar;
    //Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
  Public
    Constructor Create;
    Destructor Free;
    Procedure Hide;
    Procedure Show(NewCaption: String = '');
    Property Caption: String Read FCaption Write FCaption;
    Property DisplayHeight: Integer Read FDisplayHeight Write FDisplayHeight;
    Property DisplayWidth: Integer Read FDisplayWidth Write FDisplayWidth;
    Property Progress: Integer Read FProgress Write SetProgress;
  End;

Function CreateProgressDisplay(): TProgressDisplay;

Implementation

Function CreateProgressDisplay(): TProgressDisplay;
Begin
  Result := TProgressDisplay.Create;
End;

Constructor TProgressDisplay.Create;
Begin
  FDisplayHeight := Screen.Height Div 15;
  FDisplayWidth := Screen.Width Div 5;
  FProgressForm := TForm.Create(Nil);
  With FProgressForm Do
    Begin
      BorderStyle := bsNone;
      FormStyle := fsStayOnTop;
      OnPaint := @DoPaint;
    End;
End;

Destructor TProgressDisplay.Free;
Begin
  FProgressForm.Free;
End;

Procedure TProgressDisplay.Hide;
Begin
  With FProgressForm Do
    If Showing Then
      Begin
        Hide;
        Application.MainForm.Update;
      End;
End;

Procedure TProgressDisplay.Show(NewCaption: String = '');
Begin
  With FProgressForm Do
    Begin
      If NewCaption = '' Then
        FCaption := 'Progress'
      Else
        FCaption := NewCaption;
      If Not Showing Then
        Begin
          Application.MainForm.Update;
          SetBounds((Screen.Width-DisplayWidth) Div 2, (Screen.Height-DisplayHeight) Div 2,
                                    DisplayWidth, DisplayHeight);
          With FProgressRect Do
            Begin
              Left := 5;
              Top := 25;
              Right := ClientWidth-5;
              Bottom := ClientHeight-5;
            End;
          Show;
        End;
    End;
End;

Procedure TProgressDisplay.DoPaint(Sender: TObject);
Var
  CompletedRect: TRect;
  Style: TTextStyle;
Begin
  With FProgressForm.Canvas Do
    Begin
      Brush.Color := clDefault;
      FillRect(FProgressForm.ClientRect);
      Pen.Color := clBlack;
      Rectangle(FProgressForm.ClientRect);
      CompletedRect := FProgressRect;
      With CompletedRect Do
        Right := Left+(FProgress*(Right-Left) Div 100);
      Brush.Color := clMoneyGreen;
      FillRect(CompletedRect);
      Brush.Style := bsClear;
      Rectangle(FProgressRect);
      Font.Color := clBlack;
      Font.Size := 0;
      Font.Quality := fqCleartypeNatural;
      With Style Do
        Begin
          Alignment := taLeftJustify;
          Layout := tlTop;
          SingleLine := True;
          Clipping := True;
          ExpandTabs := False;
          ShowPrefix := False;
          Wordbreak := False;
          Opaque := False;
          SystemFont := False;
          RightToLeft := False;
          EndEllipsis := True;
        End;
      TextRect(FProgressForm.ClientRect, 5, 5, FCaption, Style);
      With Style Do
        Begin
          Alignment := taCenter;
          Layout := tlCenter;
        End;
      TextRect(FProgressRect, 0, 0, IntToStr(FProgress)+'%', Style);
    End;
End;
{       Align := alTop;
        Alignment := taRightJustify;
        Font.Color := clBlack;
        Font.Size := 10;
        //Font.Style:= [fsBold];
        Font.Quality := fqCleartypeNatural;
        Height := 20;
        Layout := tlCenter;
        Parent := ProgressForm;

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
                  //Close;
                  Hide;
                  Application.MainForm.Update;
                End;
}

Procedure TProgressDisplay.SetProgress(Value: Integer);
Begin
  If Value<0 Then
    Value := 0;
  If Value>100 Then
    Value := 100;
  If FProgress<>Value Then
    Begin
      FProgress := Value;
      FProgressForm.Repaint;
    End;
End;

End.

