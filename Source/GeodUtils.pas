Unit GeodUtils;

{ Geodesy Formatting Utilities Library.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
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
  Classes, SysUtils, StrUtils, Math, Geometry, Geodesy, GeomUtils;

Type
  TSexagesimalOption = (soPlusMinusPrefix, soNorthSouthPrefix, soEastWestPrefix, soNorthSouthSuffix, soEastWestSuffix);

Const
  SexigesimalDelims = [#0..' '];
  SexagesimalFormat = '%.0F %.0F %.4F ';
  LongSexagesimalFormat = '%.0F° %.0F'' %.4F" ';
  CompactSexagesimalFormat = '%.0FD%.0FM%.3FS';

Function FormatCoordinate(Const Coordinate: TSexagesimalCoordinate; Option: TSexagesimalOption = soPlusMinusPrefix; FormatPattern: String = SexagesimalFormat): String; Overload;
Function TryGeodeticTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Function TryGeocentricTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Function TryCartesianTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;

Implementation

Function FormatCoordinate(Const Coordinate: TSexagesimalCoordinate; Option: TSexagesimalOption = soPlusMinusPrefix; FormatPattern: String = SexagesimalFormat): String;
Begin
  With Coordinate Do
    Case Option Of
    soPlusMinusPrefix:
      If Sign=1 Then
        Result := '+'+Format(FormatPattern, [Degrees, Minutes, Seconds])
      Else
        Result := '-'+Format(FormatPattern, [Degrees, Minutes, Seconds]);
    soNorthSouthPrefix:
      If Sign=1 Then
        Result := 'N'+Format(FormatPattern, [Degrees, Minutes, Seconds])
      Else
        Result := 'S'+Format(FormatPattern, [Degrees, Minutes, Seconds]);
    soEastWestPrefix:
      If Sign=1 Then
        Result := 'E'+Format(FormatPattern, [Degrees, Minutes, Seconds])
      Else
        Result := 'W'+Format(FormatPattern, [Degrees, Minutes, Seconds]);
    soNorthSouthSuffix:
      If Sign=1 Then
        Result := Format(FormatPattern, [Degrees, Minutes, Seconds])+'N'
      Else
        Result := Format(FormatPattern, [Degrees, Minutes, Seconds])+'S';
    soEastWestSuffix:
      If Sign=1 Then
        Result := Format(FormatPattern, [Degrees, Minutes, Seconds])+'E'
      Else
        Result := Format(FormatPattern, [Degrees, Minutes, Seconds])+'W';
    End;
End;

Function TryGeodeticTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Var
  Sign: Integer;
  TextLength: Integer;
  SuffixLength: Integer;
  HasPrefix: Boolean;
  HasSuffix: Boolean;
  PartCount: Integer;
  Index: Integer;
  PartText: String;
  PartLength: Integer;
  Suffix: Char;
  DegreeValue: TCoordinate;
  MinuteValue: TCoordinate;
  SecondValue: TCoordinate;
  Function ValidHemispereLetter(Letter: Char): Boolean;
  Begin
    { Test for matching axis and change sign if required. }
    Case Letter Of
    'N': Result := (AxisType=atYAxis);
    'S':
      Begin
        Result := (AxisType=atYAxis);
        Sign := -1;
      End;
    'E': Result := (AxisType=atXAxis);
    'W':
      Begin
        Result := (AxisType=atXAxis);
        Sign := -1;
      End;
    Else
      Result := False;
    End;
  End;
Begin
  Sign := 1;
  Result := False;
  { Prepare the text for parsing. }
  SuffixLength := Length(UnitSuffix);
  Text := UpperCase(Trim(Text));
  Text := StringReplace(Text, '°', 'D', [rfReplaceAll]); { UTF8 Degree symbol. }
  Text := StringReplace(Text, #176, 'D', [rfReplaceAll]); { ANSI Degree symbol. }
  Text := StringReplace(Text, '''', 'M', [rfReplaceAll]);
  Text := StringReplace(Text, '"', 'S', [rfReplaceAll]);
  { Quit if the text is empty. }
  TextLength := Length(Text);
  If TextLength=0 Then
    Exit;
  { Process an altitude component. }
  If AxisType=atZAxis Then
    Begin
      { Remove any unit suffix. }
      If Copy(Text, 1+TextLength-SuffixLength, SuffixLength)=UpperCase(UnitSuffix) Then
        SetLength(Text, TextLength-SuffixLength);
      { Atempt to convert the remainder to a valid value and quit. }
      Result := TryStrToFloat(Text, Value);
      Exit;
    End;
  { Remove hemisphere or sign prefix, noting the sign value. }
  HasPrefix := ValidHemispereLetter(Text[1]);
  If Not HasPrefix Then
    If Text[1] In ['+','-'] Then
      Begin
        HasPrefix := True;
        If Text[1]='-' Then
          Sign := -1;
      End;
  If HasPrefix Then
    { Remove the prefix character. }
    Text := Copy(Text, 2, TextLength-1)
  Else
    Begin
      { Remove any hemisphere suffix noting the sign value. }
      HasSuffix := ValidHemispereLetter(Text[TextLength]);
      If HasSuffix Then
        Begin
          { NOTE: there is an ambiguous case where '00D 00M 00S' is used.
                  Here the seconds 'S' could be mistaken for a South suffix.
                  This is arbitrated by testing for the text containing a
                  'D' and 'M' then the 'S' is judged to be seconds. However,
                  if the last two characters are ' S' or 'SS' then the last 'S'
                  is taken to be a valid South suffix. }
          If Text[TextLength]='S' Then
            If (Pos('D', Text)<>0) And (Pos('M', Text)<>0) And
               Not ((TextLength>1) And ((Text[TextLength-1]='S') Or
                                        (Text[TextLength-1]=' '))) Then
              Begin
                Sign := 1;
                HasSuffix := False;
              End;
          { If exists, then remove the suffix and any whitespace. }
          If HasSuffix Then
            Begin
              SetLength(Text, TextLength-1);
              Text := Trim(Text);
            End;
        End;
    End;
  { Scan the parts text for invalid characters. }
  TextLength := Length(Text);
  For Index := TextLength DownTo 1 Do
    Begin
      { Quit if there are invalid characters. }
      If Not (Text[Index] In ['.'..'9',' ','D','M','S']) Then
        Exit;
      { Insert spaces between any parts run together. }
      If Not (Text[Index] In ['.'..'9', ' ']) Then
        If Index<TextLength Then
          If Text[Index+1]<>' ' Then
            Text := Copy(Text, 1, Index)+' '+Copy(Text, Index+1, TextLength);
    End;
  { Quit if there are too many parts. }
  PartCount := WordCount(Text, SexigesimalDelims);
  If PartCount>3 Then
    Exit;
  { Extract the return value parts. }
  DegreeValue := 0;
  MinuteValue := 0;
  SecondValue := 0;
  For Index := 1 To PartCount Do
    Begin
      { Extract the part word. }
      PartText := ExtractWord(Index, Text, SexigesimalDelims);
      PartLength := Length(PartText);
      { Quit if there are no numeric parts. }
      If PartLength=0 Then
        Exit;
      { Extract and remove any part suffix character. }
      If Not (PartText[PartLength] In ['0'..'9']) Then
        Begin
          HasSuffix := True;
          Suffix := PartText[PartLength];
          SetLength(PartText, PartLength-1);
        End
      Else
        HasSuffix := False;
      { Quit if decimal point in non-final numeric part. }
      If Index<>PartCount Then
        If Pos('.', PartText)<>0 Then
          Exit;
      Case Index Of
      1: { Degrees }
        Begin
          { Attempt to convert the degree part and quit if invalid. }
          If Not TryStrToFloat(PartText, DegreeValue) Then
            Exit;
          { Quit if the degree value is not within the valid range. }
          If (DegreeValue<0) Or ((DegreeValue>90) And (AxisType=atYAxis)) Or
             ((DegreeValue>180) And (AxisType=atXAxis)) Then
            Exit;
          { Quit if there is no valid degree suffix. }
          If HasSuffix And (Suffix<>'D') Then
              Exit;
        End;
      2: { Minutes }
        Begin
          { Attempt to convert the minutes part and quit if invalid. }
          If Not TryStrToFloat(PartText, MinuteValue) Then
            Exit;
          { Quit if the minute value is not within the valid range. }
          If (MinuteValue<0) Or (MinuteValue>=60) Then
            Exit;
          { Quit if there is no valid minutes suffix. }
          If HasSuffix And (Suffix<>'M') Then
            Exit;
        End;
      3: { Seconds }
        Begin
          { Attempt to convert the seconds part and quit if invalid. }
          If Not TryStrToFloat(PartText, SecondValue) Then
            Exit;
          { Quit if the second value is not within the valid range. }
          If (SecondValue<0) Or (SecondValue>=60) Then
            Exit;
          { Quit if there is no valid minutes suffix. }
          If HasSuffix And (Suffix<>'S') Then
            Exit;
        End;
      End;
    End;
  { Calculate the final return value. }
  Value := DegreeValue+MinuteValue/60+SecondValue/3600;
  { Perform a final combined value range check. }
  If ((Value>90) And (AxisType=atYAxis)) Or ((Value>180) And (AxisType=atXAxis)) Then
    Exit;
  { Re-introduce the direction sign value. }
  Value := Sign*Value;
  { Return the sucess. }
  Result := True;
End;

Function TryGeocentricTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String): Boolean;
Var
  SuffixLength: Integer;
  TextLength: Integer;
Begin
  Result := False;
  { Prepare the text for parsing. }
  SuffixLength := Length(UnitSuffix);
  Text := UpperCase(Trim(Text));
  TextLength := Length(Text);
  If TextLength>0 Then
    Begin
      { Remove any unit suffix. }
      If Copy(Text, 1+TextLength-SuffixLength, SuffixLength)=UpperCase(UnitSuffix) Then
        SetLength(Text, TextLength-SuffixLength);
      { Atempt to convert the remainder to a valid value. }
      Result := TryStrToFloat(Text, Value);
    End;
End;

Function TryCartesianTextToCoordinate(Text: String; Var Value: TCoordinate; AxisType: TAxisType; UnitSuffix: String): Boolean;
Var
  SuffixLength: Integer;
  TextLength: Integer;
  HasAxisSuffix: Boolean;
  AxisSuffix: Char;
Begin
  Result := False;
  { Prepare the text for parsing. }
  SuffixLength := Length(UnitSuffix);
  Text := UpperCase(Trim(Text));
  TextLength := Length(Text);
  If TextLength>0 Then
    Begin
      { Remove any unit suffix. }
      If Copy(Text, 1+TextLength-SuffixLength, SuffixLength)=UpperCase(UnitSuffix) Then
        SetLength(Text, TextLength-SuffixLength);
      { Validate any axis type suffix. }
      TextLength := Length(Text);
      AxisSuffix := Text[TextLength];
      HasAxisSuffix := False;
      If Not (AxisSuffix In ['0'..'9']) Then
        HasAxisSuffix := ((AxisSuffix='E') And (AxisType=atXAxis)) Or
                         ((AxisSuffix='N') And (AxisType=atYAxis));
      { Remove any axis type suffix. }
      If HasAxisSuffix Then
        SetLength(Text, TextLength-1);
      { Atempt to convert the remainder to a valid value. }
      Result := TryStrToFloat(Trim(Text), Value);
      { Treat negative values for an X or Y axis as invalid. }
      If Value<0 Then
        Result := (AxisType=atZAxis);
    End;
End;

End.

