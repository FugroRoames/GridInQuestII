Unit WKT;

{ Well Known Text Support Unit.

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
  SysUtils, StrUtils, Geometry, Geodesy;

{ WKT text constants. }
ResourceString
  rsPOLYGON = 'POLYGON';
  rsMULTIPOLYGON = 'MULTIPOLYGON';
  rsRingBreak = '),(';
  rsSeparators = '(,) '#0;
//  rsDoubleQuote = '"';
//  rsCSVBreak = ', ';
//  rsComma = ',';

Procedure WKTToMultiPolygon(WKTString: String; Var MultiPolygon: T2DCoordinatesArrayArray); {$IFDEF USE_INLINE}Inline;{$ENDIF}

Implementation

Procedure WKTToMultiPolygon(WKTString: String; Var MultiPolygon: T2DCoordinatesArrayArray); {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
  WKTStringPointer: PChar;
  RingPointer: PChar;
  RingEndPointer: PChar;
  RingCount: Integer;
  LastRingIndex: Integer;
  RingIndex: Integer;
  NodePointer: PChar;
  NodeCount: Integer;
  LastNodeIndex: Integer;
  NodeIndex: Integer;
  Procedure SkipSeparators(Var TextPointer: PChar);
  Begin
    While StrScan(PChar(rsSeparators), TextPointer^)<>Nil Do
      Inc(TextPointer);
  End;
Begin
  WKTStringPointer := PChar(WKTString);
  If StrLIComp(WKTStringPointer, PChar(rsPOLYGON), 7)=0 Then
    WKTStringPointer := WKTStringPointer+7
  Else If StrLIComp(WKTStringPointer, PChar(rsMULTIPOLYGON), 12)=0 Then
    WKTStringPointer := WKTStringPointer+12
  Else
    Begin
      SetLength(MultiPolygon, 0);
      Exit;
    End;
  RingCount := 0;
  RingPointer := WKTStringPointer;
  Repeat
    Inc(RingPointer);
    RingPointer := StrPos(RingPointer, PChar(rsRingBreak));
    Inc(RingCount);
  Until RingPointer=Nil;
  SetLength(MultiPolygon, RingCount);
  RingPointer := WKTStringPointer;
  LastRingIndex := RingCount-1;
  For RingIndex := 0 To LastRingIndex Do
    Begin
      SkipSeparators(RingPointer);
      RingEndPointer := StrScan(RingPointer,')');
      NodeCount := 0;
      NodePointer := RingPointer;
      Repeat
        Inc(NodePointer);
        NodePointer := StrScan(NodePointer,',');
        Inc(NodeCount);
      Until (NodePointer=Nil) Or (NodePointer>RingEndPointer);
      SetLength(MultiPolygon[RingIndex], NodeCount);
      NodePointer := RingPointer;
      LastNodeIndex :=  NodeCount-1;
      For NodeIndex := 0 To LastNodeIndex Do
        With MultiPolygon[RingIndex, NodeIndex] Do
          Begin
            SkipSeparators(NodePointer);
            X := StrToFloat(ExtractWord(1, NodePointer, [' ',',',')']));
            Y := StrToFloat(ExtractWord(2, NodePointer, [' ',',',')']));
            NodePointer := StrScan(NodePointer,',');
          End;
      RingPointer := StrPos(RingPointer, PChar(rsRingBreak));
      If RingPointer<>Nil Then
        SkipSeparators(RingPointer);
    End;
End;

End.

