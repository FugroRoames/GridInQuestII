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
  TFormatType = (ftDelimited, ftFixed);

Type
  TDataStream = Class(TMemoryStream)
  Private
    FBOF: Boolean;
    FCurrentRow: TStringList;
    FEOF: Boolean;
    FFieldCount: Integer;
    FFieldLengths: TFPList;
    FFieldStarts: TFPList;
    FFieldTerminator: Char;
    FFirstRow: Integer;
    FFormatType: TFormatType;
    FNameRow: Integer;
    FNames: TStringList;
    FRows: TFPList;
    FRecordCount: Integer;
    FRecordNumber: Integer;
    FTextDelimiter: Char;
    Function GetField(Index: Integer): String;
    Function GetFieldLengths(Index: Integer): Integer;
    Function GetFieldStarts(Index: Integer): Integer;
    Function GetName(Index: Integer): String;
    Function GetNamesList: String;
    Function GetValue(RecordIndex, FieldIndex: Integer): String;
    Procedure SetFieldCount(Value: Integer);
    Procedure SetFieldLengths(Index: Integer; Value: Integer);
    Procedure SetFieldStarts(Index: Integer; Value: Integer);
    Procedure SetFieldTerminator(Value: Char);
    Procedure SetFirstRow(Value: Integer);
    Procedure SetNameRow(Value: Integer);
    Procedure SetRecordNumber(Const Value: Integer);
  Protected
    FieldTerminators: TSysCharSet;
    RecordTerminators: TSysCharSet;
    Procedure ParseRow(Const Data: TStringList; Const RowIndex: Integer);
    Procedure ParseRows;
  Public
    Constructor Create;
    Constructor Create(InputStream: TStream);
    Constructor Create(FileName: String);
    Destructor Destroy; Override;
    Procedure LoadFromStream(InputStream: TStream);
    Procedure LoadFromFile(FileName: String);
    Procedure Clear;
    Procedure First;
    Procedure Last;
    Procedure Next;
    Procedure Prior;
    Property BOF: Boolean Read FBOF;
    Property EOF: Boolean Read FEOF;
    Property FieldCount: Integer Read FFieldCount Write SetFieldCount;
    Property FieldLengths[Index: Integer]: Integer Read GetFieldLengths Write SetFieldLengths;
    Property FieldStarts[Index: Integer]: Integer Read GetFieldStarts Write SetFieldStarts;
    Property Fields[Index: Integer]: String Read GetField;
    Property FieldTerminator: Char Read FFieldTerminator Write SetFieldTerminator;
    Property FirstRow: Integer Read FFirstRow Write SetFirstRow;
    Property FormatType: TFormatType Read FFormatType Write FFormatType;
    Property NameRow: Integer Read FNameRow Write SetNameRow;
    Property Names[Index: Integer]: String Read GetName;
    Property NamesList: String Read GetNamesList;
    Property RecordCount: Integer Read FRecordCount;
    Property RecordNumber: Integer Read FRecordNumber Write SetRecordNumber;
    Property TextDelimiter: Char Read FTextDelimiter Write FTextDelimiter;
    Property Values[RecordIndex, FieldIndex: Integer]: String Read GetValue; Default;
  End;

Const
  TabTerminator: Char = #9;
  CommaTerminator: Char = ',';
  StandardRecordTerminators: TSysCharSet = [#0, #10, #13];
  StandardTextDelimiter: Char = '"';

Implementation

Constructor TDataStream.Create;
Begin
  FTextDelimiter := #0;
  FCurrentRow := TStringList.Create;
  FNames := TStringList.Create;
  FRows := TFPList.Create;
  FFieldStarts := TFPList.Create;
  FFieldLengths := TFPList.Create;
  FNameRow := 0;
  FFirstRow := 1;
  FFormatType := ftDelimited;
  FieldTerminator := CommaTerminator;
  RecordTerminators := StandardRecordTerminators;
  SetFieldCount(1); { Always at least one field: the whole row of the data file. }
  Clear;
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
  FreeAndNil(FFieldStarts);
  FreeAndNil(FFieldLengths);
  FreeAndNil(FNames);
  FreeAndNil(FRows);
  Inherited Destroy;
End;

Procedure TDataStream.Clear;
Begin
  FFieldStarts.Count := 1;
  FFieldLengths.Count := 1;
  FNames.Clear;
  FRows.Clear;
  FBOF := True;
  FEOF := True;
  FRecordCount := 0;
  FRecordNumber := -1;
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
  ParseRows;
  FRecordNumber := 0;
  FBOF := False;
  FEOF := False;
End;

Procedure TDataStream.LoadFromFile(FileName: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
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
  SetRecordNumber(RecordNumber+1);
End;

Procedure TDataStream.Prior;
Begin
  SetRecordNumber(RecordNumber-1);
End;

Procedure TDataStream.SetFieldLengths(Index: Integer; Value: Integer);
Begin
  FFieldLengths[Index] := Pointer(Value);
End;

Procedure TDataStream.SetFieldStarts(Index: Integer; Value: Integer);
Begin
  FFieldStarts[Index] := Pointer(Value);
End;

Procedure TDataStream.SetFieldTerminator(Value: Char);
Begin
  If FFieldTerminator<>Value Then
    Begin
      FFieldTerminator := Value;
      FieldTerminators := StandardRecordTerminators+[Value];
    End;
End;

Procedure TDataStream.SetFirstRow(Value: Integer);
Begin
  If Value<0 Then
    Value := 0;
  If Value>=FRows.Count Then
    Value := FRows.Count-1;
  If FFirstRow<>Value Then
    Begin
      FFirstRow := Value;
      { Remove any named row setting if after new first row. }
      If FNameRow>FFirstRow Then
        FNameRow := -1;
    End;
End;

Procedure TDataStream.SetNameRow(Value: Integer);
Begin
  If Value<0 Then
    Value := -1; { Indicates no named row. }
  If Value>=FRows.Count Then
    Value := FRows.Count-1;
  If FNameRow<>Value Then
    Begin
      FNameRow := Value;
      { Force any invalid first row value to the next row after the named row. }
      If FFirstRow<=FNameRow Then
        FFirstRow := FNameRow+1;
    End;
End;

Procedure TDataStream.SetRecordNumber(Const Value: Integer);
Begin
  If RecordNumber<>Value Then
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
      ParseRow(FCurrentRow, FirstRow+RecordNumber);
    End;
End;

Function TDataStream.GetField(Index: Integer): String;
Begin
  WriteLn('GetField ', Index);
  If Index<FCurrentRow.Count Then
    Result := FCurrentRow[Index]
  Else
    Result := EmptyStr;
End;

Function TDataStream.GetFieldLengths(Index: Integer): Integer;
Begin
  Result := Integer(FFieldLengths[Index]);
End;

Function TDataStream.GetFieldStarts(Index: Integer): Integer;
Begin
  Result := Integer(FFieldStarts[Index]);
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
      LastIndex := FieldCount-1;
      For Index := 0 To LastIndex Do
        Begin
          Result := Result+GetName(Index);
          If Index<LastIndex Then
            Result := Result+LineEnding;
        End;
    End;
End;

Function TDataStream.GetValue(RecordIndex, FieldIndex: Integer): String;
Begin
  WriteLn('GetValue ', RecordIndex, ' ', FieldIndex);
  SetRecordNumber(RecordIndex);
  Result := GetField(FieldIndex);
End;

Procedure TDataStream.SetFieldCount(Value: Integer);
Begin
  If FFieldCount<>Value Then
    Begin
      If Value<1 Then
        Value := 1;
      FFieldCount := Value;
      FFieldStarts.Count := Value;
      FFieldLengths.Count := Value;
    End;
End;

Procedure TDataStream.ParseRow(Const Data: TStringList; Const RowIndex: Integer);
Var
  RecordPointer: PChar;
  CurrentPointer: PChar;
  FieldPointer: PChar;
  Index, LastIndex: Integer;
Begin
  { Clear the return data list. }
  Data.Clear;
  RecordPointer := PChar(FRows[RowIndex]);
  CurrentPointer := RecordPointer;
  { Rebuild the field breaks for each row for delimited files. }
  If FormatType=ftDelimited Then
    Begin
      Index := 0;
      While Index<FieldCount Do
        Begin
          { Move past any whitespace. }
          While CurrentPointer^=' ' Do
            Inc(CurrentPointer);
          { If a text delimiter is set, skip any delimited data. }
          If (TextDelimiter<>#0) And (FieldPointer^=TextDelimiter) Then
            Begin
              { Find the start of the delimited text. }
              Inc(CurrentPointer);
              FieldPointer := CurrentPointer;
              While Not (CurrentPointer^=TextDelimiter) Do
                Inc(CurrentPointer);
            End
          Else
            Begin
              { Find the start of the field. }
              FieldPointer := CurrentPointer;
              { Find the end of the current field. }
              While Not (CurrentPointer^ In FieldTerminators) Do
                Inc(CurrentPointer);
            End;
          { Add the extracted field extents to the index lists. }
          FieldStarts[Index] := 1+FieldPointer-RecordPointer;
          FieldLengths[Index] := CurrentPointer-FieldPointer;
          { Find the start of the next field. }
          While Not (CurrentPointer^ In FieldTerminators) Do
            Inc(CurrentPointer);
          If CurrentPointer^=FieldTerminator Then
            Inc(CurrentPointer);
          Inc(Index);
        End;
    End;
  { Build the record's data list. }
  LastIndex := FFieldCount-1;
  For Index := 0 To LastIndex Do
    Data.Add(Trim(Copy(RecordPointer, FieldStarts[Index], FieldLengths[Index])));
End;

Procedure TDataStream.ParseRows;
Var
  BufferPointer: PChar;
  BufferEndPointer: PChar;
  RowIndex: Integer;
  FoundFields: Integer;
  Procedure FindNextRecord;
  Begin
    While Not (BufferPointer^ In RecordTerminators) Do
      Inc(BufferPointer);
    If BufferPointer<BufferEndPointer Then
      While BufferPointer^ In RecordTerminators Do
        Inc(BufferPointer);
    Inc(RowIndex);
  End;
Begin
  { Prepare for the data scan. }
  Clear;
  BufferPointer := PChar(Memory);
  BufferEndPointer := BufferPointer;
  Inc(BufferEndPointer, Size);
  { Clear any control characters at the end of the data buffer. }
  While BufferEndPointer<' ' Do
    Begin
      BufferEndPointer^ := #0;
      Dec(BufferEndPointer);
    End;
  { If there is valid data to read. }
  If BufferEndPointer>BufferPointer Then
    { Build the main record index. }
    While BufferPointer<BufferEndPointer Do
      Begin
        { Add the current record to the record index list. }
        FRows.Add(BufferPointer);
        FindNextRecord;
      End;
  { Add a row record for the end of the buffer. }
  FRows.Add(BufferEndPointer);
  { For delimited files with a named row. }
  If FormatType=ftDelimited Then
    If NameRow<>-1 Then
      Begin
        { Calculate the number of fields from the header field count. }
        FoundFields := 1;
        BufferPointer := FRows[NameRow];
        While BufferPointer<FRows[NameRow+1] Do
          Begin
            If BufferPointer^=FieldTerminator Then
              Inc(FoundFields);
            Inc(BufferPointer);
          End;
        SetFieldCount(FoundFields);
        { Setup the field names list. }
        ParseRow(FNames, NameRow);
      End;
  { Set the found record count. }
  FRecordCount := FRows.Count-FirstRow-1;
  { Setup the first record. }
  ParseRow(FCurrentRow, FirstRow);
End;

End.

