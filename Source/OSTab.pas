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
  TSmallCoordinate = Single; { Defined to keep storage space of tables compact. }

Type
  TOSHorizontalModel = (hmTN02, hmTH15);
  TOSVerticalModel = (vmGM02, vmGM15);

//Type
//  TVerticalDatumCode = (vdNone, vdOrdnanceDatumNewlyn, vdStMarys, vdDouglas02,
//                        vdStornoway, vdStKilda, vdLerwick, vdNewlyn, vdFairIsle,
//                        vdFlannanIsles, vdNorthRona, vdSuleSkerry, vdFoula,
//                        vdMalinHead, vdBelfast, vdOffshore);
//
Type
  TDatumRecord = Packed Record
    Code: TVerticalDatumCode;
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
    Function Data(X, Y: Integer): THorizontalRecord;
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
    Function Data(X, Y: Integer): TVerticalRecord;
    Function LoadFromFile(FileName: String): Boolean;
    Function LoadFromStream(Source: TStream): Boolean;
  End;

Const
  VerticalData: Array [0..15] Of TDatumRecord =
    ((Code: vdNone; Name: 'None'; Region: 'Outside Model'),
    (Code: vdOrdnanceDatumNewlyn; Name: 'Ordnance Datum Newlyn'; Region: 'UK Mainland'),
    (Code: vdStMarys; Name: 'St Marys'; Region: 'Scilly Isles'),
    (Code: vdDouglas02; Name: 'Douglas02'; Region: 'Isle of Man'),
    (Code: vdStornoway; Name: 'Stornoway'; Region: 'Outer Hebrides'),
    (Code: vdStKilda; Name: 'St Kilda'; Region: 'St Kilda'),
    (Code: vdLerwick; Name: 'Lerwick'; Region: 'Shetland Isles'),
    (Code: vdNewlyn; Name: 'Newlyn'; Region: 'Orkney Isles'),
    (Code: vdFairIsle; Name: 'Fair Isle'; Region: 'Fair Isle'),
    (Code: vdFlannanIsles; Name: 'Flannan Isles'; Region: 'Flannan Isles'),
    (Code: vdNorthRona; Name: 'North Rona'; Region: 'North Rona'),
    (Code: vdSuleSkerry; Name: 'Sule Skerry'; Region: 'Sule Skerry'),
    (Code: vdFoula; Name: 'Foula'; Region: 'Foula'),
    (Code: vdMalinHead; Name: 'Malin Head'; Region: 'Republic of Ireland'),
    (Code: vdBelfast; Name: 'Belfast'; Region: 'Northern Ireland'),
    (Code: vdOffshore; Name: 'Ordnance Datum Newlyn (Offshore)'; Region: 'UK Offshore'));

Type
  TInterpolationParameters = Record
    X1, X2: Integer;
    Y1, Y2: Integer;
    T, TI: TCoordinate;
    U, UI: TCoordinate;
  End;

Function VerticalDataCodeToName(DatumCode: TVerticalDatumCode): String;
Function VerticalDataNameToCode(DatumName: String): TVerticalDatumCode;
Function BilinearGridInterpolationParameters(Const Origin: TCoordinates; Const Coordinates: TCoordinates; Const GridScale: TCoordinate): TInterpolationParameters;
Function InterpolateHorizontalTable(Const HorizontalTable: THorizontalTable; Parameters: TInterpolationParameters): TPlanarCoordinates;
Function InterpolateVerticalTable(Const VerticalTable: TVerticalTable; Parameters: TInterpolationParameters): TCoordinate;
Function ParametersValid(Parameters: TInterpolationParameters; DataHeader: TDataHeader): Boolean;

Implementation

Function VerticalDataCodeToName(DatumCode: TVerticalDatumCode): String;
Var
  Index: Integer;
Begin
  For Index := Low(VerticalData) To High(VerticalData) Do
    With VerticalData[Index] Do
      If DatumCode=Code Then
        Begin
          Result := Name;
          Exit;
        End;
  Result := EmptyStr;
End;

Function VerticalDataNameToCode(DatumName: String): TVerticalDatumCode;
Var
  Index: Integer;
Begin
  For Index := Low(VerticalData) To High(VerticalData) Do
    With VerticalData[Index] Do
      If DatumName=Name Then
        Begin
          Result := Code;
          Exit;
        End;
  Result := vdNone;
End;

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
      With Data(X1, Y1) Do
        Begin
          SE0 := ShiftEast;
          SN0 := ShiftNorth;
        End;
      With Data(X2, Y1) Do
        Begin
          SE1 := ShiftEast;
          SN1 := ShiftNorth;
        End;
      With Data(X2, Y2) Do
        Begin
          SE2 := ShiftEast;
          SN2 := ShiftNorth;
        End;
      With Data(X1, Y2) Do
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
      SG0 := Data(X1, Y1).GeoidHeight;
      SG1 := Data(X2, Y1).GeoidHeight;
      SG2 := Data(X2, Y2).GeoidHeight;
      SG3 := Data(X1, Y2).GeoidHeight;
      Result := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
    End;
End;

Function ParametersValid(Parameters: TInterpolationParameters; DataHeader: TDataHeader): Boolean;
Begin
  With Parameters, DataHeader Do
    Result := ((X1>=0) And (Y1>=0) And (X2<ColumnCount) And (Y2<RowCount));
End;

Function THorizontalTable.Data(X, Y: Integer): THorizontalRecord;
Begin
  Result := Records[X+Y*Header.ColumnCount];
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

Function TVerticalTable.Data(X, Y: Integer): TVerticalRecord;
Begin
  Result := Records[X+Y*Header.ColumnCount];
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

