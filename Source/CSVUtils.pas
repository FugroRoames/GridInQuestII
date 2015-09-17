Unit CSVUtils;

{ CSV Data Handling Unit.

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
  Classes, SysUtils, Geometry;

Type
  TCSVData = Class
  Private
    { Private declarations. }
    FColCount: Integer;
    FFileName: String;
    FNameRow: Integer;
    FRecords: TList;
    FStartRow: Integer;
    Function GetName(Col: Integer): String;
    Function GetNamesList: String;
    Function GetRowCount: Integer;
    Function GetValue(Row, Col: Integer): String;
    Procedure SetNameRow(Value: Integer);
    Procedure SetStartRow(Value: Integer);
    Procedure UpdateColCount;
  Public
    { Public declarations. }
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Function DataLoaded: Boolean;
    Function LoadFromCSVFile(FileName: String): Boolean;
    Function LoadFromCSVStream(Stream: TStream): Boolean;
    Procedure Clear;
    Property ColCount: Integer Read FColCount;
    Property NameRow: Integer Read FNameRow Write SetNameRow;
    Property Names[Col: Integer]: String Read GetName;
    Property NamesList: String Read GetNamesList;
    Property RowCount: Integer Read GetRowCount;
    Property StartRow: Integer Read FStartRow Write SetStartRow;
    Property Values[Row, Col: Integer]: String Read GetValue;
  End;

Implementation

Constructor TCSVData.Create;
Begin
  FRecords := TList.Create;
  FNameRow := -1;
  FStartRow := 0;
End;

Destructor TCSVData.Destroy;
Begin
  Clear;
  FRecords.Free;
  Inherited Destroy;
End;

Function TCSVData.DataLoaded: Boolean;
Begin
    Result := (FRecords.Count<>0);
End;

Function TCSVData.GetRowCount: Integer;
Begin
  Result := FRecords.Count-FStartRow;
End;

Function TCSVData.GetName(Col: Integer): String;
Var
  Fields: TStringList;
Begin
  If Col<FColCount Then
    If FNameRow<0 Then
      Result := 'Column '+IntToStr(Col+1)
    Else
      Begin
        Fields := TStringList(FRecords.Items[FNameRow]);
        If Assigned(Fields) Then
          Result := Fields.Strings[Col];
      End
  Else
    Result := EmptyStr;
End;

Function TCSVData.GetNamesList: String;
Var
  Col, LastCol: Integer;
Begin
  Result := EmptyStr;
  LastCol := FColCount-1;
  For Col := 0 To LastCol Do
    Begin
      Result := Result+GetName(Col);
      If Col<LastCol Then
        Result := Result+LineEnding;
    End
End;

Function TCSVData.GetValue(Row, Col: Integer): String;
Var
  Fields: TStringList;
Begin
  Result := EmptyStr;
  Fields := TStringList(FRecords.Items[Row+FStartRow]);
  If Assigned(Fields) Then
    Result := Fields.Strings[Col];
End;

Function TCSVData.LoadFromCSVFile(FileName: String): Boolean;
Var
  FileStream: TFileStream;
Begin
  If FileExists(FileName) Then
    Begin
      FileStream := TFileStream.Create(FileName, fmOpenRead+fmShareDenyWrite);
      Try
        Result := LoadFromCSVStream(FileStream);
        FFileName := FileName;
      Finally
        FileStream.Free;
      End;
    End
  Else
    Result := False;
End;

Function TCSVData.LoadFromCSVStream(Stream: TStream): Boolean;
Var
  RecordStrings: TStringList;
  Index, LastIndex: Integer;
  Fields: TStringList;
Begin
  Clear;
  RecordStrings := TStringList.Create;
  RecordStrings.LoadFromStream(Stream);
  LastIndex := RecordStrings.Count-1;
  For Index := 0 To LastIndex Do
    Begin
      Fields := TStringList.Create;
      Fields.Delimiter := ',';
      Fields.StrictDelimiter := True;
      Fields.DelimitedText := RecordStrings[Index];
      FRecords.Add(Fields);
    End;
  UpdateColCount;
  Result := DataLoaded;
  RecordStrings.Free;
End;

Procedure TCSVData.Clear;
Var
  Index, LastIndex: Integer;
  Fields: TStringList;
Begin
  LastIndex := FRecords.Count-1;
  For Index := LastIndex DownTo 0 Do
    Begin
      Fields := TStringList(FRecords.Items[Index]);
      If Assigned(Fields) Then
        Fields.Free;
    End;
  FRecords.Clear;
End;

Procedure TCSVData.SetNameRow(Value: Integer);
Begin
  If FNameRow<>Value Then
    Begin
      If Value<0 Then
        FNameRow := -1
      Else
        FNameRow := Value;
    End;
End;

Procedure TCSVData.SetStartRow(Value: Integer);
Begin
  If FStartRow<>Value Then
    Begin
      If Value<0 Then
        FStartRow := 0
      Else
        FStartRow := Value;
      UpdateColCount;
    End;
End;

Procedure TCSVData.UpdateColCount;
Var
  Fields: TStringList;
Begin
  FColCount := 0;
  If FRecords.Count>0 Then
    Begin
      Fields := TStringList(FRecords.Items[FStartRow]);
      If Assigned(Fields) Then
        FColCount := Fields.Count;
    End;
End;

End.

