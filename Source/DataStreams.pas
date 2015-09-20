Unit DataStreams;

{ Lightweight delimeted text file data reader.

  Copyright (C) 2011 Paul Michell, Michell Computing.

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
  Classes, RtlConsts, SysUtils, Strings;

Type
  TDataStream = Class(TMemoryStream)
  Private
    FBOF: Boolean;
    FCurrentRecord: PChar;
    FEOF: Boolean;
    FFieldCount: Integer;
    FRecords: TFPList;
    FRecordNumber: Integer;
    FLocked: Boolean;
    Function GetField(Index: Integer): String;
    Function GetRecordCount: Integer;
    Procedure SetRecordNumber(Const Value: Integer);
  Protected
    Function Realloc(Var NewCapacity: PtrInt): Pointer; Override;
    Procedure PrepareCurrentRecord;
  Public
    Constructor Create;
    Constructor Create(InputStream: TStream);
    Constructor Create(FileName: String);
    Destructor Destroy; Override;
    Function GetFieldValuesAsString(Const FieldNumber: Integer): String;
    Function GetFilteredFieldValuesAsString(Const FieldNumber, FilterFieldNumber: Integer; Const FilterText: String): String;
    Function Locate(Const FieldNumber: Integer; Const MatchText: String): Boolean;
    Procedure PrepareData(SkipFirstRow: Boolean);
    Procedure SortByField(FieldNumber: Integer);
    Procedure First;
    Procedure Last;
    Procedure Next;
    Procedure Prior;
    Property BOF: Boolean Read FBOF;
    Property EOF: Boolean Read FEOF;
    Property Locked: Boolean Read FLocked Write FLocked;
    Property FieldCount: Integer Read FFieldCount;
    Property RecordCount: Integer Read GetRecordCount;
    Property RecordNumber: Integer Read FRecordNumber Write SetRecordNumber;
    Property Fields[Index: Integer]: String Read GetField; Default;
  End;

Const
  DataTerminator: Char = #0;
  FieldTerminator: Char = #9; //',';
//  RecordTerminator: Char = #10;
  RecordTerminators: TSysCharSet = [#10, #13];
//  FieldTerminators: TSysCharSet = [#0, #13, ','];
  TextDelimiter: Char = '"';

Implementation

Var
  SortFieldNumber: Integer;

Constructor TDataStream.Create;
Begin
  FBOF := True;
  FEOF := True;
  FFieldCount := 0;
  FLocked := True;
  FRecords := TFPList.Create;
  FRecordNumber := -1;
  FCurrentRecord := Nil;
End;

Constructor TDataStream.Create(InputStream: TStream);
Var
  BytesRead: Int64;
  DataBuffer: Array [0..4095] Of Byte;
Begin
  Create;
  Repeat
    BytesRead := InputStream.Read(DataBuffer, SizeOf(DataBuffer));
    Write(DataBuffer, BytesRead);
  Until BytesRead = 0;
  PrepareData(True); { Skip the first data row as this contains field names. }
End;

Constructor TDataStream.Create(FileName: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  Create(FileStream);
  FileStream.Free;
End;

Destructor TDataStream.Destroy;
Begin
  FreeAndNil(FRecords);
  Inherited Destroy;
End;

procedure TDataStream.PrepareCurrentRecord;
Begin
  If FRecordNumber=-1 Then
    FCurrentRecord := Nil
  Else
    FCurrentRecord := PChar(FRecords[FRecordNumber]);
End;

function TDataStream.GetFieldValuesAsString(const FieldNumber: Integer): String;
Var
  Index, LastIndex: Integer;
Begin
  Result := EmptyStr;
  LastIndex := FRecords.Count-1;
  For Index := 0 To LastIndex Do
    Begin
      FCurrentRecord := PChar(FRecords[Index]);
      If Length(Result)>0 Then
        Result += #13#10;
      Result += Fields[FieldNumber];
    End;
  PrepareCurrentRecord;
End;

function TDataStream.GetFilteredFieldValuesAsString(const FieldNumber,
  FilterFieldNumber: Integer; const FilterText: String): String;
Var
  Index, LastIndex: Integer;
Begin
  Result := EmptyStr;
  LastIndex := FRecords.Count-1;
  For Index := 0 To LastIndex Do
    Begin
      FCurrentRecord := PChar(FRecords[Index]);
      If Fields[FilterFieldNumber]=FilterText Then
        Begin
          If Length(Result)>0 Then
            Result += #13#10;
          Result += Fields[FieldNumber];
        End;
    End;
  PrepareCurrentRecord;
End;

function TDataStream.Locate(const FieldNumber: Integer; const MatchText: String
  ): Boolean;
Var
  Index, LastIndex: Integer;
Begin
  Result := False;
  LastIndex := FRecords.Count-1;
  For Index := 0 To LastIndex Do
    Begin
      FCurrentRecord := PChar(FRecords[Index]);
      If Fields[FieldNumber]=MatchText Then
        Begin
          FRecordNumber := Index;
          Result := True;
          Exit;
        End;
    End;
  PrepareCurrentRecord;
End;

procedure TDataStream.PrepareData(SkipFirstRow: Boolean);
Var
  BufferPointer: PChar;
  BufferEnd: PChar;
  CountingFields: Boolean;
  IsDelimited: Boolean;
Begin
  FRecords.Clear;
  If Size<>0 Then
    Begin
      //TODO: Ensure all padding around qoutes is avoided.
      BufferPointer := PChar(Memory);
      BufferEnd := BufferPointer;
      Inc(BufferEnd, Size);
      { Ensure the data is zero terminated. }
      BufferEnd^ := #0;
      { Ignore any control characters or padding at end of file. }
      While BufferEnd<' ' Do
        Begin
          BufferEnd^ := #0;
          Dec(BufferEnd);
        End;
      If Not SkipFirstRow Then
        FRecords.Add(BufferPointer);
      FFieldCount := 1;
      CountingFields := True;
      IsDelimited := False;
      While BufferPointer<=BufferEnd Do
        If BufferPointer^ In RecordTerminators Then
          Begin
            CountingFields := False;
            { Advance past any other control characters or whitespace. }
            While BufferPointer<=' ' Do
              Begin
                BufferPointer^ := #0;
                Inc(BufferPointer);
              End;
            FRecords.Add(BufferPointer);
          End
        Else
          Begin
            If BufferPointer^=TextDelimiter Then
              IsDelimited := Not IsDelimited;
            If Not IsDelimited Then
              If BufferPointer^=FieldTerminator Then
                Begin
                  BufferPointer^ := #0;
                  If CountingFields Then
                    Inc(FFieldCount);
                End;
            Inc(BufferPointer);
          End;
      FBOF := False;
      FEOF := False;
      FCurrentRecord := PChar(FRecords[0]);
    End
  Else
    Begin
      FBOF := True;
      FEOF := True;
      FRecordNumber := -1;
      FCurrentRecord := Nil;
      FFieldCount := 0;
    End;
End;

Function CompareRecordPointers(Record1, Record2: Pointer): Integer;
Var
  Field1, Field2: PChar;
  Count: Integer;
Begin
  Field1 := PChar(Record1);
  Count := 0;
  While Count<SortFieldNumber Do
    Begin
      If Field1^=#0 Then
        Inc(Count);
      Inc(Field1);
    End;
  Field2 := PChar(Record2);
  Count := 0;
  While Count<SortFieldNumber Do
    Begin
      If Field2^=#0 Then
        Inc(Count);
      Inc(Field2);
    End;
  Result := strcomp(Field1, Field2);
End;

procedure TDataStream.SortByField(FieldNumber: Integer);
Begin
  If (FieldNumber>=0) And (FieldNumber<FieldCount) Then
    Begin
      SortFieldNumber := FieldNumber;
      FRecords.Sort(@CompareRecordPointers);
      FRecordNumber := 0;
      FCurrentRecord := PChar(FRecords[FRecordNumber]);
    End
  Else
    Raise EStreamError.Create('Field number out of range.');
End;

procedure TDataStream.First;
Begin
  SetRecordNumber(0);
End;

procedure TDataStream.Last;
Begin
  SetRecordNumber(RecordCount-1);
End;

procedure TDataStream.Next;
Begin
  SetRecordNumber(FRecordNumber+1);
End;

procedure TDataStream.Prior;
Begin
  SetRecordNumber(FRecordNumber-1);
End;

function TDataStream.GetRecordCount: Integer;
Begin
  Result := FRecords.Count;
End;

procedure TDataStream.SetRecordNumber(const Value: Integer);
Begin
  If FRecordNumber<>Value Then
    Begin
      FBOF := False;
      FEOF := False;
      If Value<0 Then
        Begin
          FBOF := True;
          FRecordNumber := 0;
        End
      Else If Value>=RecordCount Then
        Begin
          FEOF := True;
          FRecordNumber := RecordCount-1;
        End
      Else
        FRecordNumber := Value;
      PrepareCurrentRecord;
    End;
End;

function TDataStream.Realloc(var NewCapacity: PtrInt): Pointer;
Begin
  If NewCapacity<=0 Then
    NewCapacity := 0
  Else
    Inc(NewCapacity); { Ensure there is space for zero termination. }
  If Locked Then
    Begin
      If NewCapacity=Capacity Then
        Result := Memory
      Else
        Begin
          Result := Memory;
          Result := ReallocMem(Result, Newcapacity);
          If (Result=Nil) And (Newcapacity>0) Then
            Raise EStreamError.Create(SMemoryStreamError);
        End;
    End
  Else
    Result := Inherited Realloc(NewCapacity);
End;

function TDataStream.GetField(Index: Integer): String;
Var
  FieldNumber: Integer;
  FieldPointer: PChar;
  FieldEndPointer: PChar;
Begin
  If (Index>=0) And (Index<FFieldCount) Then
    Begin
      FieldPointer := FCurrentRecord;
      FieldNumber := 0;
      While FieldNumber<Index Do
        Begin
          If FieldPointer^=#0 Then
            Inc(FieldNumber);
          Inc(FieldPointer);
        End;
      FieldEndPointer := FieldPointer;
      While FieldEndPointer^<>#0 Do
        Inc(FieldEndPointer);
      Result := Copy(FieldPointer, 1, 1+FieldEndPointer-FieldPointer);
      If FieldPointer^='"' Then
        Result := AnsiDequotedStr(Result, '"');
    End
  Else
    Result := EmptyStr;
End;

End.

