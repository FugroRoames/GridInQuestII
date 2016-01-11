Unit GeodProc;

{ Geodesy Process Utility Unit.

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
  Math, Geometry, Geodesy, ETRS, BNG, ITM, IG, DataStreams;

Function IsSRIDGeodeticSystem(SRID: Integer): Boolean;
Function SRIDToSystemPointer(SRID: Integer): TCoordinateSystemPointer;
Function TransformCoordinates(SourceSRID, TargetSRID: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: TVerticalDatumCode): Boolean;

Implementation

Function IsSRIDGeodeticSystem(SRID: Integer): Boolean;
Var
  SystemPointer: TCoordinateSystemPointer;
Begin
  SystemPointer := SRIDToSystemPointer(SRID);
  If Assigned(SystemPointer) Then
    Result := (SystemPointer^.CoordinateType=ctGeodetic)
  Else
    Result := False;
End;

Function SRIDToSystemPointer(SRID: Integer): TCoordinateSystemPointer;
Begin
  With CoordinateSystems Do
    Result := Pointers(FindSRIDNumber(SRID));
End;

Function TransformCoordinates(SourceSRID, TargetSRID: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: TVerticalDatumCode): Boolean;
Var
  SourceSystemPointer: TCoordinateSystemPointer;
  TargetSystemPointer: TCoordinateSystemPointer;
  GeocentricCoordinates: TCoordinates;
Begin
  Result := False;
  SourceSystemPointer := SRIDToSystemPointer(SourceSRID);
  TargetSystemPointer := SRIDToSystemPointer(TargetSRID);
  If Assigned(SourceSystemPointer) Then
    GeocentricCoordinates := SourceSystemPointer^.ConvertToGeocentric(InputCoordinates)
  Else
    Exit;
  If Assigned(TargetSystemPointer) Then
    Begin
      TargetSystemPointer^.PreferredVerticalDatum := TVerticalDatumCode(DatumCode);
      OutputCoordinates := TargetSystemPointer^.ConvertFromGeocentric(GeocentricCoordinates);
      DatumCode := TargetSystemPointer^.LastVerticalDatum;
    End
  Else
    Exit;
  Result := True;
End;

End.

