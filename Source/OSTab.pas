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

Interface

Uses
  SysUtils, Classes, Math, Geometry, Geodesy;

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
  THorizontalRecordArray = Array Of THorizontalRecord;
  THorizontalTable = Packed Object
    Header: TDataHeader;
    Records: THorizontalRecordArray;
    Function Data(Row, Col: Integer): THorizontalRecord;
    Function LoadFromFile(FileName: String): Boolean;
    Function LoadFromStream(Source: TStream): Boolean;
  End;
  TVerticalRecord = Packed Record
    GeoidHeight: TSmallCoordinate;
    DatumCode: Byte;
  End;
  TVerticalRecordArray = Array Of TVerticalRecord;
  TVerticalTable = Packed Object
    Header: TDataHeader;
    Records: TVerticalRecordArray;
    Function Data(Row, Col: Integer): TVerticalRecord;
    Function LoadFromFile(FileName: String): Boolean;
    Function LoadFromStream(Source: TStream): Boolean;
  End;

Const
  VerticalDatums: Array [0..15] Of TDatumRecord =
    ((Code: 0; Name: 'None'; Region: 'Outside Model'),
    (Code: 1; Name: 'Ordnance Datum Newlyn'; Region: 'UK Mainland'),
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
    (Code: 14; Name: 'Belfast'; Region: 'Northern Ireland'),
    (Code: 15; Name: 'Ordnance Datum Newlyn (Offshore)'; Region: 'UK Offshore'));

Type
  TInterpolationParameters = Record
    X1, X2: Integer;
    Y1, Y2: Integer;
    T, TI: TCoordinate;
    U, UI: TCoordinate;
  End;

Function BilinearGridInterpolationParameters(Const Origin: TCoordinates; Const Coordinates: TCoordinates; Const GridScale: TCoordinate): TInterpolationParameters;
Function InterpolateHorizontalTable(Const HorizontalTable: THorizontalTable; Parameters: TInterpolationParameters): TPlanarCoordinates;
Function InterpolateVerticalTable(Const VerticalTable: TVerticalTable; Parameters: TInterpolationParameters): TCoordinate;

Implementation

Function BilinearGridInterpolationParameters(Const Origin: TCoordinates; Const Coordinates: TCoordinates; Const GridScale: TCoordinate): TInterpolationParameters;
Var
  InvGridScale: TCoordinate;
Begin
  InvGridScale := 1/GridScale;
  With Result Do
    Begin
      X1 := Trunc((Coordinates.Easting-Origin.Easting)*InvGridScale);
      Y1 := Trunc((Coordinates.Northing-Origin.Northing)*InvGridScale);
      X2 := X1+1;
      Y2 := Y1+1;
      T := (Coordinates.Easting-(X1*GridScale+Origin.Easting))*InvGridScale;
      TI := (1-T);
      U := (Coordinates.Northing-(Y1*GridScale+Origin.Northing))*InvGridScale;
      UI := (1-U);
    End;
End;

Function InterpolateHorizontalTable(Const HorizontalTable: THorizontalTable; Parameters: TInterpolationParameters): TPlanarCoordinates;
Var
  SE0, SE1, SE2, SE3: TCoordinate;
  SN0, SN1, SN2, SN3: TCoordinate;
Begin
  With Parameters, HorizontalTable Do
    Begin
      With Data(Y1, X1) Do
        Begin
          SE0 := ShiftEast;
          SN0 := ShiftNorth;
        End;
      With Data(Y1, X2) Do
        Begin
          SE1 := ShiftEast;
          SN1 := ShiftNorth;
        End;
      With Data(Y2, X2) Do
        Begin
          SE2 := ShiftEast;
          SN2 := ShiftNorth;
        End;
      With Data(Y2, X1) Do
        Begin
          SE3 := ShiftEast;
          SN3 := ShiftNorth;
        End;
      Result.Easting := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
      Result.Northing := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
    End;
End;

Function InterpolateVerticalTable(Const VerticalTable: TVerticalTable; Parameters: TInterpolationParameters): TCoordinate;
Var
  SG0, SG1, SG2, SG3: TCoordinate;
Begin
  With Parameters, VerticalTable Do
    Begin
      SG0 := Data(Y1, X1).GeoidHeight;
      SG1 := Data(Y1, X2).GeoidHeight;
      SG2 := Data(Y2, X2).GeoidHeight;
      SG3 := Data(Y2, X1).GeoidHeight;
      Result := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
    End;
End;

(*
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
  With HorizontalTable.Header Do
    Begin
      T := (Coordinates.Easting-(X1*GridScale+Origin.Easting))*InvGridScale;
      TI := (1-T);
      U := (Coordinates.Northing-(Y1*GridScale+Origin.Northing))*InvGridScale;
      UI := (1-U);
    End;
  EastOffset := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
  NorthOffset := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
  GeoidHeight := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
End;
*)
Function THorizontalTable.Data(Row, Col: Integer): THorizontalRecord;
Begin
  Result := Records[Col+Row*Header.ColumnCount];
End;

Function THorizontalTable.LoadFromFile(FileName: String): Boolean;
Var
  FileStream: TFileStream;
Begin
  Result := FileExists(FileName);
  If Result Then
    Begin
      FileStream := TFileStream.Create(FileName, fmOpenRead);
      Result := LoadFromStream(FileStream);
      FileStream.Free;
    End;
End;

Function THorizontalTable.LoadFromStream(Source: TStream): Boolean;
Var
  DataSize: Int64;
  RecordCount: Integer;
Begin
  DataSize := Source.Size;
  If DataSize>SizeOf(Header) Then
    Begin
      Source.Read(Header, SizeOf(Header));
      DataSize := DataSize-SizeOf(Header);
      RecordCount := DataSize Div SizeOf(THorizontalRecord);
      SetLength(Records, RecordCount);
      Source.Read(Records[0], DataSize);
      Result := True;
    End
  Else
    Result := False;
End;

Function TVerticalTable.Data(Row, Col: Integer): TVerticalRecord;
Begin
  Result := Records[Col+Row*Header.ColumnCount];
End;

Function TVerticalTable.LoadFromFile(FileName: String): Boolean;
Var
  FileStream: TFileStream;
Begin
  Result := FileExists(FileName);
  If Result Then
    Begin
      FileStream := TFileStream.Create(FileName, fmOpenRead);
      Result := LoadFromStream(FileStream);
      FileStream.Free;
    End;
End;

Function TVerticalTable.LoadFromStream(Source: TStream): Boolean;
Var
  DataSize: Int64;
  RecordCount: Integer;
Begin
  DataSize := Source.Size;
  If DataSize>SizeOf(Header) Then
    Begin
      Source.Read(Header, SizeOf(Header));
      DataSize := DataSize-SizeOf(Header);
      RecordCount := DataSize Div SizeOf(TVerticalRecord);
      SetLength(Records, RecordCount);
      Source.Read(Records[0], DataSize);
      Result := True;
    End
  Else
    Result := False;
End;

End.

