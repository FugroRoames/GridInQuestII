Unit GeodProc;

{ Geodesy Process Utility Unit.

  Copyright (C) 2016 Paul Michell, Michell Computing.

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
  Geodesy, ETRS, BNG, ITM, IG;

Function IsSRIDGeodeticSystem(SRID: Integer; Revision: Integer = 0): Boolean;
Function SRIDToSystemPointer(SRID: Integer; Revision: Integer = 0): TCoordinateSystemPointer;
Function TransformCoordinates(SourceSRID, TargetSRID: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: TVerticalDatumCode): Boolean;

Implementation

Function IsSRIDGeodeticSystem(SRID: Integer; Revision: Integer = 0): Boolean;
Var
  SystemPointer: TCoordinateSystemPointer;
Begin
  SystemPointer := SRIDToSystemPointer(SRID, Revision);
  If Assigned(SystemPointer) Then
    Result := (SystemPointer^.CoordinateType=ctGeodetic)
  Else
    Result := False;
End;

Function SRIDToSystemPointer(SRID: Integer; Revision: Integer = 0): TCoordinateSystemPointer;
Begin
  With CoordinateSystems Do
    Result := Pointers(FindSRIDNumber(SRID, Revision));
End;

Function TransformCoordinates(SourceSRID, TargetSRID: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: TVerticalDatumCode): Boolean;
Var
  SourceSystemPointer: TCoordinateSystemPointer;
  TargetSystemPointer: TCoordinateSystemPointer;
  GlobalCoordinates: TCoordinates;
Begin
  Result := False;
  { Find the source and target coordinate systems. }
  SourceSystemPointer := SRIDToSystemPointer(SourceSRID);
  TargetSystemPointer := SRIDToSystemPointer(TargetSRID);
  { Transform the input coordinates to their global representation. }
  If Assigned(SourceSystemPointer) Then
    GlobalCoordinates := SourceSystemPointer^.ConvertToGlobal(InputCoordinates)
  Else
    Exit;
  { If the target system is available. }
  If Assigned(TargetSystemPointer) Then
    Begin
      { Perform any required global type transformations. }
      Case SourceSystemPointer^.GlobalType Of
      gtCartesian:
        If TargetSystemPointer^.GlobalType=gtGeodetic Then
          GlobalCoordinates := GeocentricToGeodetic(GlobalCoordinates, GRS80Ellipsoid);
      gtGeodetic:
        If TargetSystemPointer^.GlobalType=gtCartesian Then
          GlobalCoordinates := GeodeticToGeocentric(GlobalCoordinates, GRS80Ellipsoid);
      End;
      { Transform the global coordinates to the target system. }
      TargetSystemPointer^.PreferredVerticalDatum := TVerticalDatumCode(DatumCode);
      OutputCoordinates := TargetSystemPointer^.ConvertFromGlobal(GlobalCoordinates);
      DatumCode := TargetSystemPointer^.LastVerticalDatum;
    End
  Else
    Exit;
  Result := True;
End;

End.

