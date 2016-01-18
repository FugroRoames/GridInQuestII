Unit GeoJSON;

{ GeoJSON Support Unit.

  Copyright (C) 2016 Paul Michell, Michell Computing.

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
  SysUtils, Geometry, Geodesy, fpJSON, JSONParser;

Type
  TGeoJSONType = (jtNone, jtPoint, jtMultiPoint, jtLineString, jtMultiLineString,
                  jtPolygon, jtMultiPolygon, jtGeometryCollection, jtFeature, jtFeatureCollection);

Type
  TGeoJSONTypeRecord = Packed Record
    GeoJSONType: TGeoJSONType;
    Name: String;
  End;

Const
  GeoJSONTypeData: Array [0..9] Of TGeoJSONTypeRecord =
    ((GeoJSONType: jtNone; Name: ''),
    (GeoJSONType: jtPoint; Name: 'Point'),
    (GeoJSONType: jtMultiPoint; Name: 'MultiPoint'),
    (GeoJSONType: jtLineString; Name: 'LineString'),
    (GeoJSONType: jtMultiLineString; Name: 'MultiLineString'),
    (GeoJSONType: jtPolygon; Name: 'Polygon'),
    (GeoJSONType: jtMultiPolygon; Name: 'MultiPolygon'),
    (GeoJSONType: jtGeometryCollection; Name: 'GeometryCollection'),
    (GeoJSONType: jtFeature; Name: 'Feature'),
    (GeoJSONType: jtFeatureCollection; Name: 'FeatureCollection'));

Function JSONGeometryType(Const JSONText: String): TGeoJSONType;
Function JSONPointToCoordinates(Const JSONText: String): TCoordinates;
Function JSONPointIncludesZValue(Const JSONText: String): Boolean;
Function CoordinatesToJSONPoint(Var Coordinates: TCoordinates; IncludeZAxis: Boolean = True): String;
Function CoordinatesAndAttributeToJSONPoint(Var Coordinates: TCoordinates; Var Name, Value: String; IncludeZAxis: Boolean = True): String;

Implementation

Function JSONDataToCoordinate(Const JSONData: TJSONData): TCoordinate;
Begin
  If JSONData=Nil Then
    Result := 0
  Else
    Result := JSONData.AsFloat;
End;

Function JSONGeometryType(Const JSONText: String): TGeoJSONType;
Var
  JSONData: TJSONData;
  JSONType: TJSONData;
  JSONName: String;
  Index: Integer;
Begin
  JSONData := GetJSON(JSONText);
  Try
    JSONType := JSONData.FindPath('type');
    If JSONType<>Nil Then
      Begin
        JSONName := AnsiDequotedStr(JSONType.AsJSON, '"');
        For Index := Low(GeoJSONTypeData) To High(GeoJSONTypeData) Do
          With GeoJSONTypeData[Index] Do
            If Name=JSONName Then
              Begin
                Result := GeoJSONType;
                Exit;
              End;
      End;
  Finally
    If Assigned(JSONData) Then
      JSONData.Free;
  End;
  Result := jtNone;
End;

Function JSONPointToCoordinates(Const JSONText: String): TCoordinates;
Var
  JSONData: TJSONData;
Begin
  JSONData := GetJSON(JSONText);
  Try
    Result.X := JSONDataToCoordinate(JSONData.FindPath('coordinates[0]'));
    Result.Y := JSONDataToCoordinate(JSONData.FindPath('coordinates[1]'));
    Result.Z := JSONDataToCoordinate(JSONData.FindPath('coordinates[2]'));
  Finally
    If Assigned(JSONData) Then
      JSONData.Free;
  End;
End;

Function JSONPointIncludesZValue(Const JSONText: String): Boolean;
Var
  JSONData: TJSONData;
Begin
  JSONData := GetJSON(JSONText);
  Try
    Result := (JSONData.FindPath('coordinates[2]')<>Nil);
  Finally
    If Assigned(JSONData) Then
      JSONData.Free;
  End;
End;

Function CoordinatesToJSONPoint(Var Coordinates: TCoordinates; IncludeZAxis: Boolean = True): String;
Begin
  With Coordinates Do
    If IncludeZAxis Then
      Result := Format('{"type":"Point","coordinates":[%G,%G,%G]',[X, Y, Z])
    Else
      Result := Format('{"type":"Point","coordinates":[%G,%G]}',[X, Y]);
End;

Function CoordinatesAndAttributeToJSONPoint(Var Coordinates: TCoordinates; Var Name, Value: String; IncludeZAxis: Boolean = True): String;
Begin
  With Coordinates Do
    If IncludeZAxis Then
      Result := Format('{"type":"Point","coordinates":[%G,%G,%G],"%S":%S}',[X, Y, Z, Name, Value])
    Else
      Result := Format('{"type":"Point","coordinates":[%G,%G],"%S":%S}',[X, Y, Name, Value]);
End;

End.

