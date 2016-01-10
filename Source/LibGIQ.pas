Library LibGIQ;

{ Grid InQuest II Coordinate Transformation Utility Library.

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

Uses
  {$IFDEF UNIX}
    cthreads,
    cmem,
  {$ENDIF}
  Geodesy, ETRS, BNG, IG, ITM;

{$R *.res}

Function ConvertCoordinates(SourceSRID, TargetSRID: Integer; Var InputCoordinates: TCoordinates; Var OutputCoordinates: TCoordinates; Var DatumCode: Integer): Boolean;{$IFDEF WINDOWS}StdCall;{$ELSE} CDecl;{$ENDIF}
Var
  SourcePointer: TCoordinateSystemPointer;
  TargetPointer: TCoordinateSystemPointer;
  GeocentricCoordinates: TCoordinates;
Begin
  Result := False;
  With CoordinateSystems Do
    SourcePointer := Pointers(FindSRIDNumber(SourceSRID));
  With CoordinateSystems Do
    TargetPointer := Pointers(FindSRIDNumber(TargetSRID));
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

Exports
  ConvertCoordinates;

End.

