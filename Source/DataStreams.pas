Unit DataStreams;

{ Lightweight text file data reader.

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
  Classes, RtlConsts, SysUtils, Strings;

Type
  TDataStream = Class(TMemoryStream)
  Private
    FBOF: Boolean;
    FCurrentRow: TStringList;
    FEOF: Boolean;
    FFieldCount: Integer;
    FFieldTerminator: Char;
    FFirstRow: Integer;
    FNameRow: Integer;
    FNames: TStringList;
    FRecords: TFPList;
    FRecordNumber: Integer;
    FTextDelimiter: Char;
    Function GetField(Index: Integer): String;
    Function GetName(Index: Integer): String;
    Function GetNamesList: String;
    Function GetRecordCount: Integer;
    Function GetValue(RecordIndex, FieldIndex: Integer): String;
    Procedure SetFieldTerminator(Value: Char);
    Procedure SetRecordNumber(Const Value: Integer);
  Protected
    DataStartPointer: PChar;
    DataEndPointer: PChar;
    FieldTerminators: TSysCharSet;
    RecordTerminators: TSysCharSet;
    Procedure ParseRow(Const RowStartPointer: PChar; Const Data: TStringList);
    Procedure PrepareCurrentRecord;
  Public
    Constructor Create;
    Constructor Create(InputStream: TStream);
    Constructor Create(FileName: String);
    Destructor Destroy; Override;
    Procedure LoadFromStream(InputStream: TStream);
    Procedure LoadFromFile(FileName: String);
    Procedure PrepareData;
    Procedure ResetData;
    Procedure SortByField(FieldNumber: Integer);
    Procedure First;
    Procedure Last;
    Procedure Next;
    Procedure Prior;
    Property BOF: Boolean Read FBOF;
    Property EOF: Boolean Read FEOF;
    Property FieldCount: Integer Read FFieldCount;
    Property Fields[Index: Integer]: String Read GetField; Default;
    Property FieldTerminator: Char Read FFieldTerminator Write SetFieldTerminator;
    Property FirstRow: Integer Read FFirstRow Write FFirstRow;
    Property NameRow: Integer Read FNameRow Write FNameRow;
    Property Names[Index: Integer]: String Read GetName;
    Property NamesList: String Read GetNamesList;
    Property RecordCount: Integer Read GetRecordCount;
    Property RecordNumber: Integer Read FRecordNumber Write SetRecordNumber;
    Property TextDelimiter: Char Read FTextDelimiter Write FTextDelimiter;
    Property Values[RecordIndex, FieldIndex: Integer]: String Read GetValue;
  End;

Const
  TabTerminator: TSysCharSet = [#9];
  CommaTerminator: TSysCharSet = [','];
  StandardRecordTerminators: TSysCharSet = [#0, #10, #13];
  StandardTextDelimiter: Char = '"';

Implementation

Var
  SortFieldNumber: Integer;

Constructor TDataStream.Create;
Begin
  FNameRow := 0;
  FFirstRow := 1;
  FTextDelimiter := #0;
  FCurrentRow := TStringList.Create;
  FNames := TStringList.Create;
  FRecords := TFPList.Create;
  FieldTerminators := StandardRecordTerminators+CommaTerminator;
  RecordTerminators := StandardRecordTerminators;
  ResetData;
End;

Constructor TDataStream.Create(InputStream: TStream);
Begin
  Create;
  LoadFromStream(InputStream);
End;

Constructor TDataStream.Create(FileName: String);
Begin
  Create;
  LoadFromFile(FileName);
End;

Destructor TDataStream.Destroy;
Begin
  FreeAndNil(FCurrentRow);
  FreeAndNil(FNames);
  FreeAndNil(FRecords);
  Inherited Destroy;
End;

Procedure TDataStream.LoadFromStream(InputStream: TStream);
Var
  BytesRead: Int64;
  DataBuffer: Array [0..4095] Of Byte;
Begin
  Repeat
    BytesRead := InputStream.Read(DataBuffer, SizeOf(DataBuffer));
    Write(DataBuffer, BytesRead);
  Until BytesRead = 0;
  WriteWord(0); { Ensure that the memory data is zero terminated. }
  PrepareData;
  First;
End;

Procedure TDataStream.LoadFromFile(FileName: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
End;

Procedure TDataStream.PrepareCurrentRecord;
Begin
  If FRecordNumber>-1 Then
    ParseRow(PChar(FRecords[FRecordNumber]), FCurrentRow);
End;

Procedure TDataStream.PrepareData;
Var
  BufferPointer: PChar;
  CountingFields: Boolean;
  IsDelimited: Boolean;
  RowIndex: Integer;
  Procedure FindNextRecord;
  Begin
    While Not (BufferPointer^ In RecordTerminators) Do
      Inc(BufferPointer);
    If BufferPointer<DataEndPointer Then
    While BufferPointer^ In RecordTerminators Do
      Inc(BufferPointer);
    Inc(RowIndex);
  End;
Begin
  { Prepare for the data scan. }
  ResetData;
  DataStartPointer := PChar(Memory);
  DataEndPointer := DataStartPointer;
  BufferPointer := DataStartPointer;
  CountingFields := True;
  Inc(DataEndPointer, Size);
  RowIndex := 0;
  { Clear any control characters at the end of the data buffer. }
  While DataEndPointer<' ' Do
    Begin
      DataEndPointer^ := #0;
      Dec(DataEndPointer);
    End;
  { If there is valid data to read. }
  If DataEndPointer>BufferPointer Then
    Begin
      { Validate any name row. }
      If NameRow<>-1 Then
        If FirstRow<=NameRow Then
          FirstRow := NameRow+1;
      { Skip any header rows. }
      While RowIndex<FirstRow Do
        Begin
          { If the end of buffer is reached then there are no data rows. }
          If BufferPointer>=DataEndPointer Then
            Exit;
          { Process the name row if found. }
          If RowIndex=NameRow Then
            Begin
              ParseRow(BufferPointer, FNames);
              FFieldCount := FNames.Count;
              CountingFields := False;
            End;
          FindNextRecord;
        End;
      { Build the main record index. }
      While BufferPointer<=DataEndPointer Do
        Begin
          { Parse the current record to count the fields if needed. }
          If CountingFields Then
            Begin
              FCurrentRow.Clear;
              ParseRow(BufferPointer, FCurrentRow);
              FFieldCount := FCurrentRow.Count;
              CountingFields := False;
            End;
          { Add the current record to the record index list. }
          FRecords.Add(BufferPointer);
          FindNextRecord;
        End;
    End;
End;

Procedure TDataStream.ResetData;
Begin
  FNames.Clear;
  FRecords.Clear;
  FBOF := True;
  FEOF := True;
  FRecordNumber := -1;
  FFieldCount := 0;
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

Procedure TDataStream.SortByField(FieldNumber: Integer);
Begin
  If (FieldNumber>=0) And (FieldNumber<FieldCount) Then
    Begin
      SortFieldNumber := FieldNumber;
      FRecords.Sort(@CompareRecordPointers);
      FRecordNumber := 0;
    End
  Else
    Raise EStreamError.Create('Field number out of range.');
End;

Procedure TDataStream.First;
Begin
  SetRecordNumber(0);
End;

Procedure TDataStream.Last;
Begin
  SetRecordNumber(RecordCount-1);
End;

Procedure TDataStream.Next;
Begin
  SetRecordNumber(FRecordNumber+1);
End;

Procedure TDataStream.Prior;
Begin
  SetRecordNumber(FRecordNumber-1);
End;

Function TDataStream.GetRecordCount: Integer;
Begin
  Result := FRecords.Count;
End;

Function TDataStream.GetValue(RecordIndex, FieldIndex: Integer): String;
Begin
  SetRecordNumber(RecordIndex);
  Result := GetField(FieldIndex);
End;

Procedure TDataStream.SetFieldTerminator(Value: Char);
Begin
  If FFieldTerminator<>Value Then
    Begin
      FFieldTerminator := Value;
      FieldTerminators := StandardRecordTerminators+[Value];
    End;
End;

Procedure TDataStream.SetRecordNumber(Const Value: Integer);
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

Procedure TDataStream.ParseRow(Const RowStartPointer: PChar; Const Data: TStringList);
Var
  CurrentPointer: PChar;
  FieldPointer: PChar;
  FieldLength: Integer;
Begin
  Data.Clear;
  CurrentPointer := RowStartPointer;
  While Not (CurrentPointer^ In RecordTerminators) Do
    Begin
      { Move to the first character of the new field. }
      While CurrentPointer^ In FieldTerminators Do
        Inc(CurrentPointer);
      { Move past any whitespace. }
      While CurrentPointer^=' ' Do
        Inc(CurrentPointer);
      FieldPointer := CurrentPointer;
      { If a text delimiter is set, skip any delimited data. }
      If (TextDelimiter<>#0) And (FieldPointer^=TextDelimiter) Then
        Begin
          { Find the extents of the delimited text. }
          Inc(CurrentPointer);
          FieldPointer := CurrentPointer;
          While Not (CurrentPointer^=TextDelimiter) Do
            Inc(CurrentPointer);
          { Add the extracted field to the data list. }
          FieldLength := CurrentPointer-FieldPointer;
          Data.Add(Copy(FieldPointer, 1, FieldLength));
          { Find the end of the current field. }
          While Not (CurrentPointer^ In FieldTerminators) Do
            Inc(CurrentPointer);
        End
      Else
        Begin
          { Find the end of the current field. }
          While Not (CurrentPointer^ In FieldTerminators) Do
            Inc(CurrentPointer);
          { Add the extracted field to the data list. }
          FieldLength := CurrentPointer-FieldPointer;
          Data.Add(Copy(FieldPointer, 1, FieldLength));
        End;
    End;
End;

Function TDataStream.GetField(Index: Integer): String;
Begin
  If Index<FCurrentRow.Count Then
    Result := FCurrentRow[Index];
End;

Function TDataStream.GetName(Index: Integer): String;
Begin
  Result := EmptyStr;
  If Index<FFieldCount Then
    If NameRow<0 Then
      Result := 'Column '+IntToStr(Index+1)
    Else
      If Index<FNames.Count Then
        Result := FNames[Index];
End;

Function TDataStream.GetNamesList: String;
Var
  Index, LastIndex: Integer;
Begin
  If FNames.Count>0 Then
    Result := FNames.Text
  Else
    Begin
      Result := EmptyStr;
      LastIndex := FFieldCount-1;
      For Index := 0 To LastIndex Do
        Begin
          Result := Result+GetName(Index);
          If Index<LastIndex Then
            Result := Result+LineEnding;
        End;
    End;
End;

End.

