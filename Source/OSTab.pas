Unit OSTab;

{ Ordnance Survey Interpolation Tables Unit.

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

{$INCLUDE OSTab.inc}

Interface

Uses
  Math, Geometry, Geodesy;

{ Include required TN02 array data. }
{$IFDEF OSTNO2RES_1KM}
  {$INCLUDE OSTN02R1K.inc}
{$ENDIF}
{$IFDEF OSTNO2RES_10KM}
  {$INCLUDE OSTN02R10K.inc}
{$ENDIF}
{$IFDEF OSTNO2RES_100KM}
  {$INCLUDE OSTN02R100K.inc}
{$ENDIF}

Type
  TVerticalDatumModel = (OSGM02, OSVRF10);

Type
  TSmallCoordinate = Single; { Defined to keep storage space of tables compact. }

Type
  TDatumRecord = Packed Record
    Code: Byte;
    Name: String;
    Region: String;
  End;

Type
  THorizontalRecord = Packed Record
    ShiftEast: TSmallCoordinate;
    ShiftNorth: TSmallCoordinate;
  End;
  THorizontalArray = Array Of Array Of THorizontalRecord;
  THorizontalTable = Packed Record
    Data: THorizontalArray;
    LastX: Integer;
    LastY: Integer;
  End;

Type
  TVerticalRecord = Packed Record
    GeoidHeight: TSmallCoordinate;
    DatumCode: Byte;
  End;
  TVerticalArray = Array Of Array Of TVerticalRecord;
  TVerticalTable = Packed Record
    Data: TVerticalArray;
    LastX: Integer;
    LastY: Integer;
  End;

Const
  VerticalDatums: Array [0..14] Of TDatumRecord =
    ((Code: 0; Name: ''; Region: 'Outside Model'),
    (Code: 1; Name: 'Newlyn'; Region: 'UK Mainland'),
    (Code: 2; Name: 'St Marys'; Region: 'Scilly Isles'),
    (Code: 3; Name: 'Douglas02'; Region: 'Isle of Man'),
    (Code: 4; Name: 'Stornoway'; Region: 'Outer Hebrides'),
    (Code: 5; Name: 'St Kilda'; Region: 'St Kilda'),
    (Code: 6; Name: 'Lerwick'; Region: 'Shetland Isles'),
    (Code: 7; Name: 'Newlyn'; Region: 'Orkney Isles'),
    (Code: 8; Name: 'Fair Isle'; Region: 'Fair Isle'),
    (Code: 9; Name: 'Flannan Isles'; Region: 'Flannan Isles'),
    (Code: 10; Name: 'North Rona'; Region: 'North Rona'),
    (Code: 11; Name: 'Sule Skerry'; Region: 'Sule Skerry'),
    (Code: 12; Name: 'Foula'; Region: 'Foula'),
    (Code: 13; Name: 'Malin Head'; Region: 'Republic of Ireland'),
    (Code: 14; Name: 'Belfast'; Region: 'Northern Ireland'));

{Var
  HorizontalArray: THorizontalArray =
   (((ShiftEast:0; ShiftNorth:0), (ShiftEast:0; ShiftNorth:0)),
   ((ShiftEast:0; ShiftNorth:0), (ShiftEast:0; ShiftNorth:0)));
}
Procedure BilinearInterpolate(Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);

Implementation

Procedure BilinearInterpolate(Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);
Var
  InvGridScale: TCoordinate;
  X1, X2, Y1, Y2: Integer;
  SE0, SE1, SE2, SE3: TCoordinate;
  SN0, SN1, SN2, SN3: TCoordinate;
  SG0, SG1, SG2, SG3: TCoordinate;
  T, TI, U, UI: TCoordinate;
Begin
  InvGridScale := 1/GridScale;
  X1 := Trunc(Coordinates.Easting*InvGridScale);
  Y1 := Trunc(Coordinates.Northing*InvGridScale);
  X2 := X1+1;
  Y2 := Y1+1;
  With OSTN02Data[X1, Y1] Do
    Begin
      SE0 := Easting;
      SN0 := Northing;
      SG0 := Elevation;
    End;
  With OSTN02Data[X2, Y1] Do
    Begin
      SE1 := Easting;
      SN1 := Northing;
      SG1 := Elevation;
    End;
  With OSTN02Data[X2, Y2] Do
    Begin
      SE2 := Easting;
      SN2 := Northing;
      SG2 := Elevation;
    End;
  With OSTN02Data[X1, Y2] Do
    Begin
      SE3 := Easting;
      SN3 := Northing;
      SG3 := Elevation;
    End;
  T := (Coordinates.Easting-(X1*GridScale))*InvGridScale;
  TI := (1-T);
  U := (Coordinates.Northing-(Y1*GridScale))*InvGridScale;
  UI := (1-U);
  EastOffset := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
  NorthOffset := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
  GeoidHeight := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
End;

End.

