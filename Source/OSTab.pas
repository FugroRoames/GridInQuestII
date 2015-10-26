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
  Classes, Math, Geometry, Geodesy;

{ Include required TN02 array data. }
{$IFDEF OSTNO2RES_1KM}
  { $INCLUDE OSTN02R1K.inc}
{$ENDIF}
{$IFDEF OSTNO2RES_10KM}
  { $INCLUDE OSTN02R10K.inc}
{$ENDIF}
{$IFDEF OSTNO2RES_100KM}
  { $INCLUDE OSTN02R100K.inc}
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
  TDataHeader = Packed Record
    Origin: TCoordinates;
    Step: TCoordinates;
    RowCount: Integer;
    ColumnCount: Integer;
  End;
  THorizontalRecord = Packed Record
    ShiftEast: TSmallCoordinate;
    ShiftNorth: TSmallCoordinate;
  End;
  THorizontalRecordPointer = ^THorizontalRecord;
  THorizontalTable = Packed Object
    Header: TDataHeader;
    Memory: Pointer;
    Function Data(Row, Col: Integer): THorizontalRecord;
    Procedure LoadFromFile(FileName: String);
    Procedure LoadFromStream(Source: TStream);
  End;
  TVerticalRecord = Packed Record
    GeoidHeight: TSmallCoordinate;
    DatumCode: Byte;
  End;
  TVerticalRecordPointer = ^TVerticalRecord;
  TVerticalTable = Packed Object
    Header: TDataHeader;
    Memory: Pointer;
    Function Data(Row, Col: Integer): TVerticalRecord;
    Procedure LoadFromFile(FileName: String);
    Procedure LoadFromStream(Source: TStream);
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

Procedure BilinearInterpolate(Const HorizontalTable: THorizontalTable; Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);

Implementation

Procedure BilinearInterpolate(Const HorizontalTable: THorizontalTable; Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);
Var
  InvGridScale: TCoordinate;
  X1, X2, Y1, Y2: Integer;
  SE0, SE1, SE2, SE3: TCoordinate;
  SN0, SN1, SN2, SN3: TCoordinate;
  SG0, SG1, SG2, SG3: TCoordinate;
  T, TI, U, UI: TCoordinate;
Begin
  InvGridScale := 1/GridScale;
  With HorizontalTable.Header Do
    Begin
      X1 := Trunc((Coordinates.Easting-Origin.Easting)*InvGridScale);
      Y1 := Trunc((Coordinates.Northing-Origin.Northing)*InvGridScale);
      X2 := X1+1;
      Y2 := Y1+1;
      If (X1<0) Or (Y1<0) Or (X2>=ColumnCount) Or (Y2>=RowCount) Then
        Begin
          EastOffset := 0;
          NorthOffset := 0;
          GeoidHeight := 0;
          Exit;
        End;
    End;
  With HorizontalTable.Data(Y1, X1) Do
    Begin
      SE0 := ShiftEast;
      SN0 := ShiftNorth;
      SG0 := 0;
    End;
  With HorizontalTable.Data(Y1, X2) Do
    Begin
      SE1 := ShiftEast;
      SN1 := ShiftNorth;
      SG1 := 0;
    End;
  With HorizontalTable.Data(Y2, X2) Do
    Begin
      SE2 := ShiftEast;
      SN2 := ShiftNorth;
      SG2 := 0;
    End;
  With HorizontalTable.Data(Y2, X1) Do
    Begin
      SE3 := ShiftEast;
      SN3 := ShiftNorth;
      SG3 := 0;
    End;
  T := (Coordinates.Easting-(X1*GridScale))*InvGridScale;
  TI := (1-T);
  U := (Coordinates.Northing-(Y1*GridScale))*InvGridScale;
  UI := (1-U);
  EastOffset := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
  NorthOffset := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
  GeoidHeight := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
End;

Function THorizontalTable.Data(Row, Col: Integer): THorizontalRecord;
Var
  RecordPointer: THorizontalRecordPointer;
Begin
  RecordPointer := THorizontalRecordPointer(Memory);
  Inc(RecordPointer, Col+Row*Header.ColumnCount);
  Result := RecordPointer^;
End;

Procedure THorizontalTable.LoadFromFile(FileName: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
End;

Procedure THorizontalTable.LoadFromStream(Source: TStream);
Var
  DataSize: Int64;
Begin
  DataSize := Source.Size;
  Source.Read(Header, SizeOf(Header));
  DataSize := DataSize-SizeOf(Header);
  Memory := AllocMem(DataSize);
  Source.Read(Memory, DataSize);
End;

Function TVerticalTable.Data(Row, Col: Integer): TVerticalRecord;
Var
  RecordPointer: TVerticalRecordPointer;
Begin
  RecordPointer := TVerticalRecordPointer(Memory);
  Inc(RecordPointer, Col+Row*Header.ColumnCount);
  Result := RecordPointer^;
End;

Procedure TVerticalTable.LoadFromFile(FileName: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
End;

Procedure TVerticalTable.LoadFromStream(Source: TStream);
Var
  DataSize: Int64;
Begin
  DataSize := Source.Size;
  Source.Read(Header, SizeOf(Header));
  DataSize := DataSize-SizeOf(Header);
  Memory := AllocMem(DataSize);
  Source.Read(Memory, DataSize);
End;

End.
