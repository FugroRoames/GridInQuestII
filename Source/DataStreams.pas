Unit DataStreams;

{ Lightweight text file data reader.

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
  Classes, SysUtils;

Type
  TFormatType = (ftDelimited, ftFixed);

Type
  TOnProgressEvent = Procedure(Sender: TObject; Progress: Integer) Of Object;

Type
  TDataStream = Class(TMemoryStream)
  Private
    FBOF: Boolean;
    FConsecutiveDelimiters: Boolean;
    FCurrentRow: TStringList;
    FEOF: Boolean;
    FFieldCount: Integer;
    FFieldLengths: TFPList;
    FFieldStarts: TFPList;
    FFieldTerminator: Char;
    FFileName: String;
    FFirstRow: Integer;
    FFormatType: TFormatType;
    FLastRow: Integer;
    FNameRow: Integer;
    FNames: TStringList;
    FOnLoadProgress: TOnProgressEvent;
    FOnParseProgress: TOnProgressEvent;
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
    Procedure SetConsecutiveDelimiters(Value: Boolean);
    Procedure SetFieldCount(Value: Integer);
    Procedure SetFieldLengths(Index: Integer; Value: Integer);
    Procedure SetFieldStarts(Index: Integer; Value: Integer);
    Procedure SetFieldTerminator(Value: Char);
    Procedure SetFirstRow(Value: Integer);
    Procedure SetLastRow(Value: Integer);
    Procedure SetNameRow(Value: Integer);
    Procedure SetRecordNumber(Const Value: Integer);
  Protected
    FieldTerminators: TSysCharSet;
    RecordTerminators: TSysCharSet;
    Procedure ParseRow(Const Data: TStringList; Const RowIndex: Integer);
    Procedure ParseRows;
    Procedure ParseFields;
    Procedure UpdateRecordCount;
  Public
    MinProgressSize: Int64;
    Constructor Create;
    Constructor Create(InputStream: TStream);
    Constructor Create(FileName: String);
    Destructor Destroy; Override;
    Procedure LoadFromStream(InputStream: TStream);
    Procedure LoadFromFile(FileName: String);
    Procedure First;
    Procedure Last;
    Procedure Next;
    Procedure Prior;
    Function FieldAsText(Index: Integer; TextDelimiter: Char): String;
    Function NameAsText(Index: Integer; TextDelimiter: Char = #0): String;
    Function NamesAsText(OutputFieldTerminator: Char; TextDelimiter: Char = #0): String;
    Function RecordAsText(OutputFieldTerminator: Char; TextDelimiter: Char = #0): String;
    Function RowLength(RowIndex: Integer): Integer;
    Property BOF: Boolean Read FBOF;
    Property ConsecutiveDelimiters: Boolean Read FConsecutiveDelimiters Write SetConsecutiveDelimiters;
    Property EOF: Boolean Read FEOF;
    Property FieldCount: Integer Read FFieldCount Write SetFieldCount;
    Property FieldLengths[Index: Integer]: Integer Read GetFieldLengths Write SetFieldLengths;
    Property FieldStarts[Index: Integer]: Integer Read GetFieldStarts Write SetFieldStarts;
    Property Fields[Index: Integer]: String Read GetField;
    Property FieldTerminator: Char Read FFieldTerminator Write SetFieldTerminator;
    Property FileName: String Read FFileName;
    Property FirstRow: Integer Read FFirstRow Write SetFirstRow;
    Property FormatType: TFormatType Read FFormatType Write FFormatType;
    Property LastRow: Integer Read FLastRow Write SetLastRow;
    Property NameRow: Integer Read FNameRow Write SetNameRow;
    Property Names[Index: Integer]: String Read GetName;
    Property NamesList: String Read GetNamesList;
    Property OriginalFileName: String Read FFileName;
    Property RecordCount: Integer Read FRecordCount;
    Property RecordNumber: Integer Read FRecordNumber Write SetRecordNumber;
    Property TextDelimiter: Char Read FTextDelimiter Write FTextDelimiter;
    Property Values[RecordIndex, FieldIndex: Integer]: String Read GetValue; Default;
    Property OnLoadProgress: TOnProgressEvent Read FOnLoadProgress Write FOnLoadProgress;
    Property OnParseProgress: TOnProgressEvent Read FOnParseProgress Write FOnParseProgress;
  End;

Const
  TabTerminator: Char = #9;
  CommaTerminator: Char = ',';
  DefaultMinProgressSize: Int64 = 10485760; { 10Mb }
  StandardRecordTerminators: TSysCharSet = [#0, #10, #13];
  StandardTextDelimiter: Char = '"';

Implementation

constructor TDataStream.Create;
Begin
  FTextDelimiter := #0;
  FCurrentRow := TStringList.Create;
  FNames := TStringList.Create;
  FRows := TFPList.Create;
  FFieldStarts := TFPList.Create;
  FFieldLengths := TFPList.Create;
  FNameRow := 0;
  FFirstRow := 1;
  FLastRow := -1; { Unlimited flag. }
  FFormatType := ftDelimited;
  FieldTerminator := CommaTerminator;
  MinProgressSize := DefaultMinProgressSize;
  RecordTerminators := StandardRecordTerminators;
  SetFieldCount(1); { Always at least one field: the whole row of the data file. }
End;

constructor TDataStream.Create(InputStream: TStream);
Begin
  Create;
  LoadFromStream(InputStream);
End;

constructor TDataStream.Create(FileName: String);
Begin
  Create;
  LoadFromFile(FileName);
End;

destructor TDataStream.Destroy;
Begin
  FreeAndNil(FCurrentRow);
  FreeAndNil(FFieldStarts);
  FreeAndNil(FFieldLengths);
  FreeAndNil(FNames);
  FreeAndNil(FRows);
  Inherited Destroy;
End;

procedure TDataStream.LoadFromStream(InputStream: TStream);
Var
  BytesRead: Int64;
  BytesLoaded: Int64;
  DataSize: Int64;
  DataBuffer: Array [0..8188] Of Byte;
  Progress, LastProgress: Integer;
Begin
  BytesLoaded := 0;
  Progress := 0;
  LastProgress := 0;
  { If load progress events are required. }
  If Assigned(FOnLoadProgress) Then
      Begin
        DataSize := InputStream.Size;
        { If the data file is large enough, send the first load event. }
        If DataSize>MinProgressSize Then
          FOnLoadProgress(Self, 0);
      End;
  Clear;
  Repeat
    BytesRead := InputStream.Read(DataBuffer, SizeOf(DataBuffer));
    Inc(BytesLoaded, BytesRead);
    Write(DataBuffer, BytesRead);
    { If load progress events are required, send the events. }
    If Assigned(FOnLoadProgress) Then
      If DataSize>MinProgressSize Then
        Begin
          Progress := Integer((99*BytesLoaded) Div DataSize);
          { Limit the progress reporting to 99 as 100 will be sent once as the last event below. }
          If Progress>99 Then
            Progress := 99;
          If Progress>LastProgress Then
            FOnLoadProgress(Self, Progress);
          LastProgress := Progress;
        End;
  Until BytesRead = 0;
  WriteWord(0); { Ensure that the memory data is zero terminated. }
  { If load progress events are required, send the last event. }
  If Assigned(FOnLoadProgress) Then
    Begin
      FOnLoadProgress(Self, 100);
    End;
  ParseRows;
End;

procedure TDataStream.LoadFromFile(FileName: String);
Var
  FileStream: TFileStream;
  Extension: String;
Begin
  FFileName := FileName;
  Extension := UpperCase(ExtractFileExt(FileName));
  If Extension='.TAB' Then
    FieldTerminator := #9;
  If Extension='.CSV' Then
    TextDelimiter := '"';
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
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
  SetRecordNumber(RecordNumber+1);
End;

procedure TDataStream.Prior;
Begin
  SetRecordNumber(RecordNumber-1);
End;

Function TDataStream.FieldAsText(Index: Integer; TextDelimiter: Char): String;
Begin
  If TextDelimiter=#0 Then
    Result := Fields[Index]
  Else
    Result := AnsiQuotedStr(Fields[Index], TextDelimiter);
End;

Function TDataStream.NameAsText(Index: Integer; TextDelimiter: Char): String;
Begin
  If TextDelimiter=#0 Then
    Result := GetName(Index)
  Else
    Result := AnsiQuotedStr(GetName(Index), TextDelimiter);
End;

Function TDataStream.NamesAsText(OutputFieldTerminator: Char; TextDelimiter: Char): String;
Var
  Index, LastIndex: Integer;
Begin
  Result := EmptyStr;
  LastIndex := FieldCount-1;
  For Index := 0 To LastIndex Do
    Begin
      Result := Result+NameAsText(Index, TextDelimiter);
      If Index<LastIndex Then
        Result := Result+OutputFieldTerminator;
    End;
End;

Function TDataStream.RecordAsText(OutputFieldTerminator: Char; TextDelimiter: Char): String;
Var
  Index, LastIndex: Integer;
Begin
  Result := EmptyStr;
  LastIndex := FieldCount-1;
  For Index := 0 To LastIndex Do
    Begin
      Result := Result+FieldAsText(Index, TextDelimiter);
      If Index<LastIndex Then
        Result := Result+OutputFieldTerminator;
    End;
End;

function TDataStream.RowLength(RowIndex: Integer): Integer;
Begin
  Result := FRows[RowIndex+1]-FRows[RowIndex]
End;

procedure TDataStream.SetFieldLengths(Index: Integer; Value: Integer);
Begin
  FFieldLengths[Index] := Pointer(Value);
End;

procedure TDataStream.SetFieldStarts(Index: Integer; Value: Integer);
Begin
  FFieldStarts[Index] := Pointer(Value);
End;

procedure TDataStream.SetFieldTerminator(Value: Char);
Begin
  If FFieldTerminator<>Value Then
    Begin
      FFieldTerminator := Value;
      FieldTerminators := StandardRecordTerminators+[Value];
      ParseFields;
    End;
End;

procedure TDataStream.SetFirstRow(Value: Integer);
Begin
  If Value<0 Then
    Value := 0;
  If FRows.Count>0 Then
    If Value>=FRows.Count Then
      Value := FRows.Count-1;
  If FFirstRow<>Value Then
    Begin
      FFirstRow := Value;
      { Remove any named row setting if at or after the new first row. }
      If FNameRow>=FFirstRow Then
        FNameRow := -1;
      UpdateRecordCount;
    End;
End;

procedure TDataStream.SetLastRow(Value: Integer);
Begin
  If FLastRow<>Value Then
    Begin
      If Value=-1 Then { Treat -1 as a flag for unlimited. }
        FLastRow := Value
      Else
        Begin
          If (Value<0) Or (Value>=FRows.Count) Then
            Value := FRows.Count-1;
          FLastRow := Value;
          { If Last row is before the first row, make them equal. }
          If FLastRow<FFirstRow Then
            FLastRow := FFirstRow;
        End;
      UpdateRecordCount;
    End;
End;

procedure TDataStream.SetNameRow(Value: Integer);
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
      UpdateRecordCount;
      ParseFields;
    End;
End;

procedure TDataStream.SetRecordNumber(const Value: Integer);
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

function TDataStream.GetField(Index: Integer): String;
Begin
  If Index<FCurrentRow.Count Then
    Result := FCurrentRow[Index]
  Else
    Result := EmptyStr;
End;

function TDataStream.GetFieldLengths(Index: Integer): Integer;
Begin
  Result := Integer(FFieldLengths[Index]);
End;

function TDataStream.GetFieldStarts(Index: Integer): Integer;
Begin
  Result := Integer(FFieldStarts[Index]);
End;

function TDataStream.GetName(Index: Integer): String;
Begin
  Result := EmptyStr;
  If Index<FFieldCount Then
    If NameRow<0 Then
      Result := 'Column '+IntToStr(Index+1)
    Else
      If Index<FNames.Count Then
        Result := FNames[Index];
End;

function TDataStream.GetNamesList: String;
Var
  Index, LastIndex: Integer;
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

function TDataStream.GetValue(RecordIndex, FieldIndex: Integer): String;
Begin
  SetRecordNumber(RecordIndex);
  Result := GetField(FieldIndex);
End;

procedure TDataStream.SetConsecutiveDelimiters(Value: Boolean);
Begin
  If FConsecutiveDelimiters<>Value Then
    Begin
      FConsecutiveDelimiters := Value;
      ParseFields;
    End;
End;

procedure TDataStream.SetFieldCount(Value: Integer);
Begin
  If FFieldCount<>Value Then
    Begin
      If Value<1 Then
        Value := 1;
      FFieldCount := Value;
      FFieldStarts.Count := Value;
      FFieldLengths.Count := Value;
      ParseFields;
    End;
End;

procedure TDataStream.ParseRow(const Data: TStringList; const RowIndex: Integer);
Label
  SkipDoubleDelimiter;
Var
  RecordPointer: PChar;
  CurrentPointer: PChar;
  FieldPointer: PChar;
  FieldIndex, LastFieldIndex: Integer;
  FieldText: String;
  Index: Integer;
Begin
  { Clear the return data list. }
  Data.Clear;
  RecordPointer := PChar(FRows[RowIndex]);
  CurrentPointer := RecordPointer;
  { Rebuild the field breaks for each row for delimited files. }
  If FormatType=ftDelimited Then
    Begin
      FieldIndex := 0;
      While FieldIndex<FieldCount Do
        Begin
          { If the end of record is reached due to insufficient fields found. }
          If CurrentPointer^ In RecordTerminators Then
            Begin
              { Add a place holder empty field value. }
              FieldStarts[FieldIndex] := 0;
              FieldLengths[FieldIndex] := 0;
            End
          Else
            Begin
              { Find the start of the field. }
              FieldPointer := CurrentPointer;
              { Move past any whitespace. }
              While CurrentPointer^=' ' Do
                Inc(CurrentPointer);
              { If a text delimiter is set, skip any delimited data. }
              If (TextDelimiter<>#0) And (CurrentPointer^=TextDelimiter) Then
                Begin
SkipDoubleDelimiter:
                  { Move past the delimiter. }
                  Inc(CurrentPointer);
                  { Look for the terminating delimiter. }
                  While CurrentPointer^<>TextDelimiter Do
                    Inc(CurrentPointer);
                  { Move past the delimiter. }
                  Inc(CurrentPointer);
                  { If a double delimiter is found, continue searching for the true terminator. }
                  If CurrentPointer^=TextDelimiter Then
                    Goto SkipDoubleDelimiter;
                End
              Else
                Begin
                  { Find the end of the current field. }
                  While Not (CurrentPointer^ In FieldTerminators) Do
                    Inc(CurrentPointer);
                End;
              { Add the extracted field extents to the FieldIndex lists. }
              FieldStarts[FieldIndex] := 1+FieldPointer-RecordPointer;
              FieldLengths[FieldIndex] := CurrentPointer-FieldPointer;
              If ConsecutiveDelimiters Then
                Begin
                  { Find the start of the next field. }
                  While Not (CurrentPointer^ In FieldTerminators) Do
                    Inc(CurrentPointer);
                  { Skip multiple field terminators. }
                  While CurrentPointer^=FieldTerminator Do
                    Inc(CurrentPointer);
                End
              Else
                Begin
                  { Find the start of the next field. }
                  While Not (CurrentPointer^ In FieldTerminators) Do
                    Inc(CurrentPointer);
                  If CurrentPointer^=FieldTerminator Then
                    Inc(CurrentPointer);
                End;
            End;
          Inc(FieldIndex);
        End;
    End;
  { Build the record's data list. }
  LastFieldIndex := FFieldCount-1;
  For FieldIndex := 0 To LastFieldIndex Do
    Begin
      CurrentPointer := RecordPointer;
      Inc(CurrentPointer, FieldStarts[FieldIndex]-1);
      SetLength(FieldText, FieldLengths[FieldIndex]);
      For Index := 1 To FieldLengths[FieldIndex] Do
        Begin
          FieldText[Index] := CurrentPointer^;
          Inc(CurrentPointer);
        End;
      { Isolate the current field's text. }
      If TextDelimiter=#0 Then
        FieldText := Trim(FieldText)
      Else
        FieldText := AnsiDequotedStr(Trim(FieldText), TextDelimiter);
      Data.Add(FieldText);
    End;
  { Note: The above loop block is equivolent to:

    Data.Add(Trim(Copy(RecordPointer, FieldStarts[FieldIndex], FieldLengths[FieldIndex])));
    or
    Data.Add(AnsiDequotedStr(Trim(Copy(RecordPointer, FieldStarts[FieldIndex], FieldLengths[FieldIndex])), TextDelimiter));

    However, Copy performs poorly on large data files, hence the direct character copying above.
    This code will need further work to accomodate UTF8 extended characters. }
End;

procedure TDataStream.ParseRows;
Var
  BufferPointer: PChar;
  BufferStartPointer: PChar;
  BufferEndPointer: PChar;
  Progress, LastProgress: Integer;
  Procedure FindNextRecord;
  Begin
    While Not (BufferPointer^ In RecordTerminators) Do
      Inc(BufferPointer);
    If BufferPointer<BufferEndPointer Then
      While BufferPointer^ In RecordTerminators Do
        Inc(BufferPointer);
  End;
Begin
  { If parse progress events are required, send the first event. }
  If Assigned(FOnParseProgress) Then
    If Size>MinProgressSize Then
      FOnParseProgress(Self, 0);
  { Prepare for the data scan. }
  FFieldCount := 1;
  FFieldStarts.Count := 1;
  FFieldLengths.Count := 1;
  FCurrentRow.Clear;
  FNames.Clear;
  FRows.Clear;
  FRecordCount := 0;
  BufferPointer := PChar(Memory);
  BufferStartPointer := BufferPointer;
  BufferEndPointer := BufferPointer;
  Inc(BufferEndPointer, Size);
  { Clear any control characters at the end of the data buffer. }
  While BufferEndPointer<' ' Do
    Begin
      BufferEndPointer^ := #0;
      Dec(BufferEndPointer);
    End;
  LastProgress := 0;
  { If there is valid data to read. }
  If BufferEndPointer>BufferPointer Then
    { Build the main record index. }
    While BufferPointer<BufferEndPointer Do
      Begin
        { If parse progress events are required. }
        If Assigned(FOnParseProgress) Then
          If Size>MinProgressSize Then
            Begin
              Progress := Integer((99*(Int64(BufferPointer)-Int64(BufferStartPointer))) Div Size);
              { Limit the progress reporting to 99 as 100 will be sent once as the last event below. }
              If Progress>99 Then
                Progress := 99;
              If Progress>LastProgress Then
                FOnParseProgress(Self, Progress);
              LastProgress := Progress;
            End;
        { Add the current record to the record index list. }
        FRows.Add(BufferPointer);
        FindNextRecord;
      End;
  { Add a row record for the end of the buffer. }
  FRows.Add(BufferEndPointer);
  { Calculate the record count. }
  UpdateRecordCount;
  { Analyse the field structure. }
  ParseFields;
  { Setup the first record if there are records. }
  If FRecordCount>0 Then
    Begin
      FBOF := False;
      FEOF := False;
      ParseRow(FCurrentRow, FirstRow);
    End
  Else
    Begin
      FBOF := True;
      FEOF := True;
    End;
  { If parse progress events are required, send the final event. }
  If Assigned(FOnParseProgress) Then
    FOnParseProgress(Self, 100);
End;

procedure TDataStream.ParseFields;
Var
  FoundFields: Integer;
  Function CountRowFields(RowIndex: Integer): Integer;
  Var
    BufferPointer: PChar;
  Begin
    Result := 1;
    BufferPointer := FRows[RowIndex];
    While BufferPointer<FRows[RowIndex+1] Do
      Begin
        { Find the start of the next field. }
        While Not (BufferPointer^ In FieldTerminators) Do
          Begin
            { If there is a text delimiter in use and a delimiter is encountered. }
            If (TextDelimiter<>#0) And (BufferPointer^=TextDelimiter) Then
              Begin
                { Move passed the delimiter. }
                Inc(BufferPointer);
                { Skip all characters until the next delimiter or out of characters. }
                While BufferPointer^<>TextDelimiter Do
                  Begin
                    Inc(BufferPointer);
                    If BufferPointer>=FRows[RowIndex+1] Then
                      Break;
                  End;
                { Skip the final text delimiter. }
                Inc(BufferPointer);
              End
            Else
              { Otherwise advance to the next character. }
              Inc(BufferPointer);
          End;
        { If the end of field is found. }
        If BufferPointer^=FieldTerminator Then
          { Increment the field count result. }
          Inc(Result);
        { Skip any field terminators if needed. }
        If ConsecutiveDelimiters Then
          While BufferPointer^ In FieldTerminators Do
            Inc(BufferPointer)
        Else
          Inc(BufferPointer);
      End;
  End;
  Procedure UpdateFieldCount(NewCount: Integer);
  Begin
    If NewCount<1 Then
      NewCount := 1;
    FFieldCount := NewCount;
    FFieldStarts.Count := NewCount;
    FFieldLengths.Count := NewCount;
  End;
Begin
  { If there is loaded data. }
  If FRecordCount>0 Then
    { For delimited files with a named row. }
    If FormatType=ftDelimited Then
      If NameRow<>-1 Then
        Begin
          { Calculate the number of fields from the header field count. }
          FoundFields := CountRowFields(NameRow);
          UpdateFieldCount(FoundFields);
          { Setup the field names list. }
          ParseRow(FNames, NameRow);
        End
      Else
        Begin
          { Otherwise count the fields in the first row. }
          FoundFields := CountRowFields(FirstRow);
          UpdateFieldCount(FoundFields);
          { If the last row is known. }
          If LastRow<>-1 Then
            Begin
              { Double check by counting the fields in the last row and use that if greater. }
              FoundFields := CountRowFields(LastRow);
              If FoundFields>FieldCount Then
                UpdateFieldCount(FoundFields);
            End;
        End;
End;

procedure TDataStream.UpdateRecordCount;
Begin
  { Set the found record count, or truncate to the last row if one given. }
  FRecordCount := FRows.Count-FirstRow-1;
  If LastRow<>-1 Then
    If 1+LastRow-FirstRow<FRecordCount Then
      FRecordCount := 1+LastRow-FirstRow;
End;

End.

