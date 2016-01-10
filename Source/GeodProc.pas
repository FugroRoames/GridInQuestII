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
  Math, Geometry, Geodesy, ETRS, BNG, ITM, IG;

Function TransformCoordinates(SourceEPSG, TargetEPSG: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: Integer): Boolean;

Implementation

Function TransformCoordinates(SourceEPSG, TargetEPSG: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: Integer): Boolean;
Var
  SourcePointer: TCoordinateSystemPointer;
  TargetPointer: TCoordinateSystemPointer;
  GeocentricCoordinates: TCoordinates;
Begin
  Result := False;
  With CoordinateSystems Do
    SourcePointer := Pointers(FindEPSGNumber(SourceEPSG));
  With CoordinateSystems Do
    TargetPointer := Pointers(FindEPSGNumber(TargetEPSG));
  If Assigned(SourcePointer) Then
    GeocentricCoordinates := SourcePointer^.ConvertToGeocentric(InputCoordinates)
  Else
    Exit;
  If Assigned(TargetPointer) Then
    Begin
      TargetPointer^.PreferredVerticalDatum := TVerticalDatumCode(DatumCode);
      OutputCoordinates := TargetPointer^.ConvertFromGeocentric(GeocentricCoordinates);
      DatumCode := Integer(TargetPointer^.LastVerticalDatum);
    End
  Else
    Exit;
  Result := True;
End;

End.

