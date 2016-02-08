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
  Classes, SysUtils, StrUtils, Geometry, Geodesy, GeomUtils;

Type
  TTypedOption = (toSignPrefix, toLetterPrefix, toLetterSuffix, toTwoPartSexagseimal, toThreePartSexagseimal, toGeodeticLetters, toGeodeticSymbols,
                  toCompactWhitespace, toPositiveLongitude, toUnitSuffix, toHeightDatumSuffix);
  TTypedOptions = Set Of TTypedOption;

Function FormatTypedCoordinate(Const Coordinate: TCoordinate; CoordinateType: TCoordinateType; AxisType: TAxisType;
                               DecimalPlaces: Integer = -1; Options: TTypedOptions = []; DatumSuffix: String = ''): String;
Function FormatTypedCoordinates(Const Coordinates: TCoordinates; CoordinateType: TCoordinateType; AxisOrder: TAxisOrder;
                                DecimalPlaces: Integer = -1; Options: TTypedOptions = []; DatumSuffix: String = ''; Const Spacer: String = ' '): String;
Function TryTextToCoordinate(Text: String; Var Coordinate: TCoordinate; CoordinateType: TCoordinateType; AxisType: TAxisType): Boolean;
Function TryGeodeticTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Function TryCartesianTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Function TryProjectedTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;

Implementation

Const
  SexigesimalDelims = [#0..' '];
  DecimalFormat = '%.*F';
  TwoPartSexagesimalFormat = '%.0F %.*F';
  ThreePartSexagesimalFormat = '%.0F %.0F %.*F';
  DecimalSymbolFormat = '%.*F°';
  TwoPartSexagesimalSymbolFormat = '%.0F° %.*F''';
  ThreePartSexagesimalSymbolFormat = '%.0F° %.0F'' %.*F"';
  DecimalLetterFormat = '%.*FD';
  TwoPartSexagesimalLetterFormat = '%.0FD %.*FM';
  ThreePartSexagesimalLetterFormat = '%.0FD %.0FM %.*FS';

Function FormatTypedCoordinate(Const Coordinate: TCoordinate; CoordinateType: TCoordinateType; AxisType: TAxisType;
                               DecimalPlaces: Integer; Options: TTypedOptions = []; DatumSuffix: String = ''): String;
Var
  ValidatedCoordinate: TCoordinate;
  Prefix: String;
  Suffix: String;
  Spacer: String;
  Function AdjustWhitespace(Text: String): String;
  Begin
    If toCompactWhitespace In Options Then
      Result := AnsiReplaceStr(Text, ' ', '')
    Else
      Result := Text;
  End;
  Function FormatGeodeticCoordinate: String;
  Var
    CurrentFormat: String;
    Indicatior: String;
    SexagesimalCoordinate: TSexagesimalCoordinate;
  Begin
    { Construct requested coordinate options format. }
    If toThreePartSexagseimal In Options Then
      Begin
        If toGeodeticSymbols In Options Then
          CurrentFormat := AdjustWhitespace(ThreePartSexagesimalSymbolFormat)
        Else If toGeodeticLetters In Options Then
          CurrentFormat := AdjustWhitespace(ThreePartSexagesimalLetterFormat)
        Else
          CurrentFormat := ThreePartSexagesimalFormat;
        SexagesimalCoordinate := DecimalToSexagesimalCoordinate(ValidatedCoordinate);
        With SexagesimalCoordinate Do
          Result := Format(CurrentFormat, [Degrees, Minutes, DecimalPlaces, Seconds]);
      End
    Else If toTwoPartSexagseimal In Options Then
      Begin
        If toGeodeticSymbols In Options Then
          CurrentFormat := AdjustWhitespace(TwoPartSexagesimalSymbolFormat)
        Else If toGeodeticLetters In Options Then
          CurrentFormat := AdjustWhitespace(TwoPartSexagesimalLetterFormat)
        Else
          CurrentFormat := TwoPartSexagesimalFormat;
        SexagesimalCoordinate := DecimalToSexagesimalCoordinate(ValidatedCoordinate, True); { Ignore the seconds part. }
        With SexagesimalCoordinate Do
          Result := Format(CurrentFormat, [Degrees, DecimalPlaces, Minutes]);
      End
    Else
      Begin
        If toGeodeticSymbols In Options Then
          CurrentFormat := DecimalSymbolFormat
        Else If toGeodeticLetters In Options Then
          CurrentFormat := DecimalLetterFormat
        Else
          CurrentFormat := DecimalFormat;
        SexagesimalCoordinate := DecimalToSexagesimalCoordinate(ValidatedCoordinate); { Required to isolate sign. }
        Result := Format(CurrentFormat, [DecimalPlaces, Abs(ValidatedCoordinate)]); { Sign applied later. }
      End;
    { Determine required prefix and suffix characters. }
    If AxisType=atXAxis Then
      If SexagesimalCoordinate.Sign=1 Then
        Indicatior := 'E'
      Else
        Indicatior := 'W'
    Else If AxisType=atYAxis Then
      If SexagesimalCoordinate.Sign=1 Then
        Indicatior := 'N'
      Else
        Indicatior := 'S';
    If toLetterSuffix In Options Then
      Suffix := Indicatior
    Else If toLetterPrefix In Options Then
      Prefix := Indicatior
    Else
      { Otherwise apply any required sign character. }
      If SexagesimalCoordinate.Sign=-1 Then
        Result := '-'+Result
      Else
        If toSignPrefix In Options Then
          Result := '+'+Result;
  End;
  Function FormatVerticalCoordinate: String;
  Var
    UnitText: String;
  Begin
    If toUnitSuffix In Options Then
      UnitText := 'm' //TODO: Lookup from coordinate system properties or additional parameter?
    Else
      UnitText := '';
    Result := FormatCoordinateWithUnits(ValidatedCoordinate, UnitText, DecimalPlaces, True);
    If toHeightDatumSuffix In Options Then
      If DatumSuffix<>'' Then
        Suffix := '['+DatumSuffix+']';
  End;
  Function FormatGeneralCoordinate(Identifier: String): String;
  Begin
    If toLetterPrefix In Options Then
      Prefix := Identifier
    Else If toLetterSuffix In Options Then
      Suffix := Identifier;
    Result := FormatCoordinate(ValidatedCoordinate, DecimalPlaces, True);
  End;
Begin
  ValidatedCoordinate := Coordinate;
  Prefix := '';
  Suffix := '';
  If toCompactWhitespace In Options Then
    Spacer := ''
  Else
    Spacer := ' ';
  Case CoordinateType Of
  ctGeodetic:
    Case AxisType Of
    atXAxis:
      Begin
        If toPositiveLongitude In Options Then
          If Coordinate<0 Then
            ValidatedCoordinate := 360+Coordinate;
        Result := FormatGeodeticCoordinate;
      End;
    atYAxis: Result := FormatGeodeticCoordinate;
    atZAxis: Result := FormatVerticalCoordinate;
    End;
  ctProjected:
    Case AxisType Of
    atXAxis: Result := FormatGeneralCoordinate('E');
    atYAxis: Result := FormatGeneralCoordinate('N');
    atZAxis: Result := FormatVerticalCoordinate;
    End;
  ctCartesian:
    Case AxisType Of
    atXAxis: Result := FormatGeneralCoordinate('X');
    atYAxis: Result := FormatGeneralCoordinate('Y');
    atZAxis: Result := FormatGeneralCoordinate('Z');
    End;
  End;
  Result := Trim(Prefix+Spacer+Result+Spacer+Suffix);
End;

Function FormatTypedCoordinates(Const Coordinates: TCoordinates; CoordinateType: TCoordinateType; AxisOrder: TAxisOrder;
                                DecimalPlaces: Integer; Options: TTypedOptions = []; DatumSuffix: String = ''; Const Spacer: String = ' '): String;
Begin
  Case AxisOrder Of
  aoXYZ:
    Result := FormatTypedCoordinate(Coordinates.X, CoordinateType, atXAxis, DecimalPlaces, Options)+Spacer+
              FormatTypedCoordinate(Coordinates.Y, CoordinateType, atYAxis, DecimalPlaces, Options)+Spacer+
              FormatTypedCoordinate(Coordinates.Z, CoordinateType, atZAxis, DecimalPlaces, Options, DatumSuffix);
  aoYXZ:
    Result := FormatTypedCoordinate(Coordinates.Y, CoordinateType, atYAxis, DecimalPlaces, Options)+Spacer+
              FormatTypedCoordinate(Coordinates.X, CoordinateType, atXAxis, DecimalPlaces, Options)+Spacer+
              FormatTypedCoordinate(Coordinates.Z, CoordinateType, atZAxis, DecimalPlaces, Options, DatumSuffix);
  End;
End;

Function TryHeightTextToCoordinate(Text: String; Var Coordinate: TCoordinate; UnitSuffix: String): Boolean;
Var
  TextLength: Integer;
  SectionLength: Integer;
Begin
  { Remove any datum suffix. }
  SectionLength := Pos('[', Text)-1;
  If SectionLength>0 Then
    SetLength(Text, SectionLength);
  { Remove any unit suffix. }
  Text := Trim(Text);
  TextLength := Length(Text);
  SectionLength := Length(UnitSuffix);
  If SameText(Copy(Text, 1+TextLength-SectionLength, SectionLength), UnitSuffix) Then
    SetLength(Text, TextLength-SectionLength);
  { Atempt to convert the remainder to a valid value and quit. }
  Text := Trim(Text);
  Result := TryStrToFloat(Text, Coordinate);
End;

Function TryTextToCoordinate(Text: String; Var Coordinate: TCoordinate; CoordinateType: TCoordinateType; AxisType: TAxisType): Boolean;
Begin
  Case CoordinateType Of
  ctCartesian:
    Result := TryCartesianTextToCoordinate(Text, Coordinate, AxisType);
  ctGeodetic:
    Result := TryGeodeticTextToCoordinate(Text, Coordinate, AxisType);
  ctProjected:
    Result := TryProjectedTextToCoordinate(Text, Coordinate, AxisType);
  Else
    Result := False;
  End;
End;

Function TryGeodeticTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String = 'm'): Boolean;
Var
  Sign: Integer;
  TextLength: Integer;
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
        If Result Then
          Sign := -1;
      End;
    'E': Result := (AxisType=atXAxis);
    'W':
      Begin
        Result := (AxisType=atXAxis);
        If Result Then
          Sign := -1;
      End;
    Else
      Result := False;
    End;
  End;
Begin
  Sign := 1;
  Result := False;
  { Quit if the text is empty. }
  Text := Trim(Text);
  TextLength := Length(Text);
  If TextLength=0 Then
    Exit;
  { Process an altitude component. }
  If AxisType=atZAxis Then
    Begin
      Result := TryHeightTextToCoordinate(Text, Coordinate, UnitSuffix);
      Exit;
    End;
  { Prepare the text for parsing geodetic axes. }
  Text := UpperCase(Trim(Text));
  Text := StringReplace(Text, '°', 'D', [rfReplaceAll]); { UTF8 Degree symbol. }
  Text := StringReplace(Text, #176, 'D', [rfReplaceAll]); { ANSI Degree symbol. }
  Text := StringReplace(Text, '''', 'M', [rfReplaceAll]);
  Text := StringReplace(Text, '"', 'S', [rfReplaceAll]);
  TextLength := Length(Text);
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
      { NOTE: there is an ambiguous case where '00D 00M 00S' is used.
              Here the seconds 'S' could be mistaken for a South suffix.
              This is arbitrated by testing for the text containing a
              'D' and 'M' then the 'S' is judged to be seconds. In this
              case there must be a second 'S' for the last to be taken
              as a valid South suffix. }
      If HasSuffix Then
        If Text[TextLength]='S' Then
          Begin
            { If there are units for degrees and minutes. }
            If (Pos('D', Text)<>0) And (Pos('M', Text)<>0) Then
              { If there is no second 'S' character. }
              If RPosEx('S', Text, TextLength-1)=0 Then
                Begin
                  { Reset the status to no suffix. }
                  Sign := 1;
                  HasSuffix := False;
                End;
          End;
      { If exists, then remove the suffix and any whitespace. }
      If HasSuffix Then
        Begin
          SetLength(Text, TextLength-1);
          Text := Trim(Text);
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
  Coordinate := DegreeValue+MinuteValue/60+SecondValue/3600;
  { Perform the final Y axis range check. }
  If AxisType=atYAxis Then
    If Coordinate>90 Then
      Exit;
  { Re-introduce the direction sign value. }
  Coordinate := Sign*Coordinate;
  { Perform the final X axis range check. }
  If AxisType=atXAxis Then
    Begin
      If Coordinate<-180 Then
        Exit;
      If Coordinate>360 Then
        Exit;
    End;
  { Return the sucess. }
  Result := True;
End;

Function TryCartesianTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String): Boolean;
Var
  SuffixLength: Integer;
  TextLength: Integer;
Begin
  Result := False;
  { Prepare the text for parsing. }
  SuffixLength := Length(UnitSuffix);
  Text := UpperCase(Text);
  { Remove any valid prefix or suffix characters. }
  Case AxisType Of
  atXAxis: Text := StringReplace(Text, 'X', '', [rfReplaceAll, rfIgnoreCase]);
  atYAxis: Text := StringReplace(Text, 'Y', '', [rfReplaceAll, rfIgnoreCase]);
  atZAxis: Text := StringReplace(Text, 'Z', '', [rfReplaceAll, rfIgnoreCase]);
  End;
  Text := Trim(Text);
  TextLength := Length(Text);
  If TextLength>0 Then
    Begin
      { Remove any unit suffix. }
      If Copy(Text, 1+TextLength-SuffixLength, SuffixLength)=UpperCase(UnitSuffix) Then
        SetLength(Text, TextLength-SuffixLength);
      { Atempt to convert the remainder to a valid value. }
      Result := TryStrToFloat(Text, Coordinate);
    End;
End;

Function TryProjectedTextToCoordinate(Text: String; Var Coordinate: TCoordinate; AxisType: TAxisType; UnitSuffix: String): Boolean;
Var
  SectionLength: Integer;
  TextLength: Integer;
Begin
  { Process any elevation component. }
  If AxisType=atZAxis Then
    Begin
      Result := TryHeightTextToCoordinate(Text, Coordinate, UnitSuffix);
      Exit;
    End;
  { Remove any valid prefix or suffix characters. }
  Case AxisType Of
  atXAxis: Text := StringReplace(Text, 'E', '', [rfReplaceAll, rfIgnoreCase]);
  atYAxis: Text := StringReplace(Text, 'N', '', [rfReplaceAll, rfIgnoreCase]);
  End;
  { Prepare the text for parsing projected axes. }
  SectionLength := Length(UnitSuffix);
  Text := UpperCase(Trim(Text));
  TextLength := Length(Text);
  If TextLength>0 Then
    Begin
      { Remove any unit suffix. }
      If Copy(Text, 1+TextLength-SectionLength, SectionLength)=UpperCase(UnitSuffix) Then
        SetLength(Text, TextLength-SectionLength);
      { Atempt to convert the remainder to a valid value. }
      Result := TryStrToFloat(Trim(Text), Coordinate);
      { Treat negative values for an X or Y axis as invalid. }
      If Coordinate<0 Then
        Result := (AxisType=atZAxis);
    End
  Else
    Result := False;
End;

End.

