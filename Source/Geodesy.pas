Unit Geodesy;

{ Geodesy Computation Library.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details. }

{ Usage Note:
  Geodesy must follow Geometry on the Uses clause for the redefinition of
  TCoordinates to be visible in code. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Math, Geometry;

Type
  TCoordinateType = (ctGeocentric, ctGeodetic, ctCartesian);

Type
  TAxisOrder = (aoXYZ, aoYXZ);

Type
  TAxisType = (atXAxis, atYAxis, atZAxis);

Type
  TAxisNames = Packed Record
    LongX: String;
    LongY: String;
    LongZ: String;
    ShortX: String;
    ShortY: String;
    ShortZ: String;
  End;

Type
  TPlanarCoordinates = Packed Object(Geometry.T2DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
  End;

Type
  TCoordinates = Packed Object(Geometry.T3DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Latitude: TCoordinate Read Y Write Y;
    Property Longitude: TCoordinate Read X Write X;
    Property Altitude: TCoordinate Read Z Write Z;
  End;

Type
  TMeasuredCoordinates = Packed Object(Geometry.T3DCoordinates)
    M: TCoordinate;
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Measure: TCoordinate Read M Write M;
  End;

Type TExtents = Packed Object
    P1, P2: TCoordinates;
    Property Min: TCoordinates Read P1 Write P1;
    Property Max: TCoordinates Read P2 Write P2;
    Property SouthWest: TCoordinates Read P1 Write P1;
    Property NorthEast: TCoordinates Read P2 Write P2;
    Property Western: TCoordinate Read P1.X Write P1.X;
    Property Southern: TCoordinate Read P1.Y Write P1.Y;
    Property Eastern: TCoordinate Read P2.X Write P2.X;
    Property Northern: TCoordinate Read P2.Y Write P2.Y;
  End;

Type
  TSexagesimalCoordinate = Packed Object
      Degrees, Minutes, Seconds, Sign: TCoordinate;
  End;

Type TSexagesimalCoordinates = Packed Object
    Latitude, Longitude: TSexagesimalCoordinate;
    Altitude: TCoordinate;
  End;

Type THelmertTransformParameters = Packed Object
    Translation: TCoordinates;
    Rotation: TCoordinates;
    Scale: TCoordinate;
  End;

Type TEllipsoid = Packed Object
    SemiMajorAxis: TCoordinate;
    SemiMinorAxis: TCoordinate;
    SemiMajorAxisSquared: TCoordinate;
    SemiMinorAxisSquared: TCoordinate;
    Eccentricity: TCoordinate;
    EccentricitySquared: TCoordinate;
    Constructor Initialize(NewSemiMajorAxis, NewSemiMinorAxis: TCoordinate);
  End;

Type TProjection = Packed Object
    Ellipsoid: TEllipsoid;
    OriginOffset: TCoordinates;
    MeridianScaleFactor: TCoordinate;
    TrueOrigin: TCoordinates;
    Constructor Initialize(NewMeridianScaleFactor, NewTrueOriginLatitude, NewTrueOriginLongitude, NewOriginOffsetEasting, NewOriginOffsetNorthing: TCoordinate; NewEllipsoid: TEllipsoid);
  End;

Const
  OneOverSixty: TCoordinate = 1/60;
  OneOverSixtySquared: TCoordinate = 1/(60*60);
  NullCoordinates: TCoordinates = (X: 0; Y: 0; Z: 0);

Function GeodeticDegToRad(Const Coordinates: TCoordinates): TCoordinates;
Function GeodeticRadToDeg(Const Coordinates: TCoordinates): TCoordinates;
Function AxisTypeFromIndex(Index: Integer; AxisOrder: TAxisOrder = aoXYZ): TAxisType;
Function SexagesimalToDecimalCoordinate(Const Coordinate: TSexagesimalCoordinate): TCoordinate;
Function DecimalToSexagesimalCoordinate(Const Coordinate: TCoordinate): TSexagesimalCoordinate;
Function SexagesimalToDecimalCoordinates(Const Coordinates: TSexagesimalCoordinates): TCoordinates;
Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates): TSexagesimalCoordinates;
Function GeodeticToGeocentric(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid):TCoordinates;
Function GeocentricToGeodetic(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid):TCoordinates;
Function HelmertTransform(Const Coordinates: TCoordinates; Const HelmertTransformParameters: THelmertTransformParameters): TCoordinates;
Function TransverseMercatorProjection(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;

Const
  GeocentricAxisNames: TAxisNames = (LongX: 'X Coordinate'; LongY: 'Y Coordinate'; LongZ: 'Z Coordinate';
                                     ShortX: 'X'; ShortY: 'Y'; ShortZ: 'Z');
  GeodeticAxisNames: TAxisNames = (LongX: 'Longitude'; LongY: 'Latitude'; LongZ: 'Altitude';
                                   ShortX: 'Long'; ShortY: 'Lat'; ShortZ: 'Alt');
  CartesianAxisNames: TAxisNames = (LongX: 'Easting'; LongY: 'Northing'; LongZ: 'Elevation';
                                    ShortX: 'East'; ShortY: 'North'; ShortZ: 'Height');

Type TCoordinateSystem = Object
    Abbreviation: String;
    AxisOrder: TAxisOrder;
    CoordinateType: TCoordinateType;
    Description: String;
    EPSGNumber: Integer;
    Name: String;
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String;
                         NewEPSGNumber: Integer; NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder);
    Function AxisNames: TAxisNames;
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual;
  End;

Type TCoordinateSystemPointer = ^TCoordinateSystem;

Type TCoordinateSystems = Object
    Count: Integer;
    Function AvailableSystemsList(OmitIndex: Integer = -1): String;
    Function FindByDescription(Const Description: String): Integer;
    Function FindEPSGNumber(Const Number: Integer): Integer;
    Function IndexOf(Const CoordinateSystem: TCoordinateSystem): Integer;
    Function Items(Index: Integer): TCoordinateSystem;
    Function Pointers(Index: Integer): TCoordinateSystemPointer;
    Procedure Register(Const CoordinateSystem: TCoordinateSystem);
  End;

Var
  CoordinateSystems: TCoordinateSystems;

Implementation

Function GeodeticDegToRad(Const Coordinates: TCoordinates): TCoordinates;
Begin
  Result.Latitude := DegToRad(Coordinates.Latitude);
  Result.Longitude := DegToRad(Coordinates.Longitude);
End;

Function GeodeticRadToDeg(Const Coordinates: TCoordinates): TCoordinates;
Begin
  Result.Latitude := RadToDeg(Coordinates.Latitude);
  Result.Longitude := RadToDeg(Coordinates.Longitude);
End;

Function AxisTypeFromIndex(Index: Integer; AxisOrder: TAxisOrder): TAxisType;
Begin
  Case AxisOrder Of
  aoXYZ:
    Case Index Of
    0: Result := atXAxis;
    1: Result := atYAxis;
    2: Result := atZAxis;
    End;
  aoYXZ:
    Case Index Of
    0: Result := atYAxis;
    1: Result := atXAxis;
    2: Result := atZAxis;
    End;
  End;
End;

Function SexagesimalToDecimalCoordinate(Const Coordinate: TSexagesimalCoordinate): TCoordinate;
Begin;
  With Coordinate Do
    Result := Sign*(Degrees+Minutes*OneOverSixty+Seconds*OneOverSixtySquared);
End;

Function DecimalToSexagesimalCoordinate(Const Coordinate: TCoordinate): TSexagesimalCoordinate;
Var
  Value: TCoordinate;
Begin
  With Result Do
    Begin
      Sign := Math.Sign(Coordinate);
      Value := Abs(Coordinate);
      Degrees := Int(Value);
      Value := Frac(Value)*60;
      Minutes := Int(Value);
      Seconds := Frac(Value)*60;
    End;
End;

Function SexagesimalToDecimalCoordinates(Const Coordinates: TSexagesimalCoordinates): TCoordinates;
Begin
  With Result Do
    Begin
      Latitude := SexagesimalToDecimalCoordinate(Coordinates.Latitude);
      Longitude := SexagesimalToDecimalCoordinate(Coordinates.Longitude);
      Altitude := Coordinates.Altitude;
    End;
End;

Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates): TSexagesimalCoordinates;
Begin
  With Result Do
    Begin
      Latitude := DecimalToSexagesimalCoordinate(Coordinates.Latitude);
      Longitude := DecimalToSexagesimalCoordinate(Coordinates.Longitude);
      Altitude := Coordinates.Altitude;
    End;
End;

Function GeodeticToGeocentric(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid):TCoordinates;
Var
  Height: TCoordinate;
  SinLat, CosLat: Extended;
  SinLon, CosLon: Extended;
Begin
  With Coordinates, Ellipsoid Do
    Begin
      SinCos(Latitude, SinLat, CosLat);
      SinCos(Longitude, SinLon, CosLon);
      Height := SemiMajorAxis/Sqrt(1-(EccentricitySquared*SinLat*SinLat));
      Result.X := (Height+Altitude)*CosLat*CosLon;
      Result.Y := (Height+Altitude)*CosLat*SinLon;
      Result.Z := ((1-EccentricitySquared)*Height+Altitude)*SinLat;
    End;
End;

Function GeocentricToGeodetic(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid): TCoordinates;
Var
  Param: TCoordinate;
  Height: TCoordinate;
  LatValue, LastLatValue: TCoordinate;
  SinLat: TCoordinate;
Const
  Epsilon = 1E-14;
Begin
  With Coordinates, Ellipsoid Do
    Begin
      Result.Longitude := ArcTan(Y/X);
      Param := Sqrt(X*X+Y*Y);
      LastLatValue := 0;
      LatValue := ArcTan(Z/(Param*(1-EccentricitySquared)));
      While Abs(LatValue-LastLatValue)>Epsilon Do
        Begin
          SinLat := Sin(LatValue);
          Height := SemiMajorAxis/Sqrt(1-(EccentricitySquared*SinLat*SinLat));
          LatValue := ArcTan((Z+(EccentricitySquared*Height*SinLat))/Param);
          LastLatValue := LatValue;
        End;
      Result.Latitude := LatValue;
      Result.Altitude := (Param/Cos(LatValue))-Height;
    End;
End;

Function HelmertTransform(Const Coordinates: TCoordinates; Const HelmertTransformParameters: THelmertTransformParameters): TCoordinates;
Var
  ScalePlusOne: TCoordinate;
Begin
  With HelmertTransformParameters Do
    Begin
      ScalePlusOne := Scale+1;
      Result.X := Translation.X+(Coordinates.X*ScalePlusOne-Coordinates.Y*Rotation.Z+Coordinates.Z*Rotation.Y);
      Result.Y := Translation.Y+(Coordinates.X*Rotation.Z+Coordinates.Y*ScalePlusOne-Coordinates.Z*Rotation.X);
      Result.Z := Translation.Z+(-Coordinates.X*Rotation.Y+Coordinates.Y*Rotation.X+Coordinates.Z*ScalePlusOne);
    End;
End;

Function TransverseMercatorProjection(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;
Var
  SinLatitude: TCoordinate;
  SinLatitudePow2: TCoordinate;
  CosLatitude: TCoordinate;
  CosLatitudePow2: TCoordinate;
  CosLatitudePow3: TCoordinate;
  CosLatitudePow5: TCoordinate;
  TanLatitude: TCoordinate;
  TanLatitudePow2: TCoordinate;
  TanLatitudePow3: TCoordinate;
  TanLatitudePow4: TCoordinate;
  n, n2, n3, o: TCoordinate;
  Nu: TCoordinate;
  Rho: TCoordinate;
  NetaSquared: TCoordinate;
  M: TCoordinate;
  I: TCoordinate;
  II: TCoordinate;
  III: TCoordinate;
  IIIA: TCoordinate;
  IV: TCoordinate;
  V: TCoordinate;
  VI: TCoordinate;
  LatitudeDelta: TCoordinate;
  LatitudeSum: TCoordinate;
  Term1: TCoordinate;
  Term2: TCoordinate;
  Term3: TCoordinate;
  Term4: TCoordinate;
  L, L2, L3, L4, L5, L6: TCoordinate;
Begin
  With Coordinates, Projection, Projection.Ellipsoid Do
    Begin
      SinLatitude := Sin(Latitude);
      SinLatitudePow2 := SinLatitude*SinLatitude;
      CosLatitude := Cos(Latitude);
      CosLatitudePow2 := CosLatitude*CosLatitude;
      CosLatitudePow3 := CosLatitudePow2*CosLatitude;
      CosLatitudePow5 := CosLatitudePow3*CosLatitudePow2;
      TanLatitude := SinLatitude/CosLatitude;
      TanLatitudePow2 := TanLatitude*TanLatitude;
      TanLatitudePow3 := TanLatitudePow2*TanLatitude;
      TanLatitudePow4 := TanLatitudePow3*TanLatitude;
      n := (SemiMajorAxis-SemiMinorAxis)/(SemiMajorAxis+SemiMinorAxis);
      n2 := n*n;
      n3 := n2*n;
      o := 1-EccentricitySquared*SinLatitudePow2;
      Nu := SemiMajorAxis*MeridianScaleFactor*Power(o, -0.5);
      Rho := SemiMajorAxis*MeridianScaleFactor*(1-EccentricitySquared)*Power(o, -1.5);
      NetaSquared := (Nu/rho)-1.0;
      LatitudeDelta := Latitude-TrueOrigin.Latitude;
      LatitudeSum := Latitude+TrueOrigin.Latitude;
      Term1 := (1.0+n+5.0/4.0*(n2+n3))*LatitudeDelta;// TODO: Possible removal of division here?
      Term2 := (3.0*(n+n2)+21.0/8.0*n3)*Sin(LatitudeDelta)*Cos(LatitudeSum);
      Term3 := 15.0/8.0*(n2+n3)*Sin(2.0*LatitudeDelta)*Cos(2.0*LatitudeSum);
      Term4 := 35.0/24.0*n3*Sin(3.0*LatitudeDelta)*Cos(3.0*LatitudeSum);
      M := SemiMinorAxis*MeridianScaleFactor*(Term1-Term2+Term3-Term4);
      I := M+OriginOffset.Northing;
      II := (Nu/2.0)*SinLatitude*CosLatitude;
      III := (Nu/24.0)*SinLatitude*CosLatitudePow3*(5.0-TanLatitudePow2+9.0*NetaSquared);
      IIIA := (Nu/720.0)*SinLatitude*CosLatitudePow5*(61.0-58.0*TanLatitudePow2+TanLatitudePow4);
      IV := Nu*CosLatitude;
      V := (Nu/6.0)*CosLatitudePow3*(Nu/rho-TanLatitudePow2);
      VI := (Nu/120.0)*CosLatitudePow5*(5.0-18.0*TanLatitudePow2+TanLatitudePow4+14.0*NetaSquared-58.0*TanLatitudePow2*NetaSquared);
      L := TrueOrigin.Longitude+Longitude;
      L2 := L*L;
      L3 := L2*L;
      L4 := L3*L;
      L5 := L4*L;
      L6 := L5*L;
      Result.Northing := I+II*L2+III*L4+IIIA*L6;
      Result.Easting := OriginOffset.Easting-(IV*L+V*L3+VI*L5);
      Result.Elevation := Altitude; { Preserve any altitude information. }
    End;
End;

Constructor TProjection.Initialize(NewMeridianScaleFactor, NewTrueOriginLatitude,
                                   NewTrueOriginLongitude, NewOriginOffsetEasting,
                                   NewOriginOffsetNorthing: TCoordinate; NewEllipsoid: TEllipsoid);
Begin
  MeridianScaleFactor := NewMeridianScaleFactor;
  TrueOrigin.Latitude := NewTrueOriginLatitude;
  TrueOrigin.Longitude := NewTrueOriginLongitude;
  OriginOffset.Easting := NewOriginOffsetEasting;
  OriginOffset.Northing := NewOriginOffsetNorthing;
  Ellipsoid := NewEllipsoid;
End;

Constructor TEllipsoid.Initialize(NewSemiMajorAxis, NewSemiMinorAxis: TCoordinate);
Begin
  SemiMajorAxis := NewSemiMajorAxis;
  SemiMinorAxis := NewSemiMinorAxis;
  SemiMajorAxisSquared := SemiMajorAxis*SemiMajorAxis;
  SemiMinorAxisSquared := SemiMinorAxis*SemiMinorAxis;
  Eccentricity := (SemiMajorAxis-SemiMinorAxis)/SemiMajorAxis;
  EccentricitySquared := (SemiMajorAxisSquared-SemiMinorAxisSquared)/SemiMajorAxisSquared;
End;

Constructor TCoordinateSystem.Initialize(NewName: String; NewAbbreviation: String; NewDescription: String;
                            NewEPSGNumber: Integer; NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder);
Begin
  Name := NewName;
  Abbreviation := NewAbbreviation;
  Description := NewDescription;
  EPSGNumber := NewEPSGNumber;
  CoordinateType := NewCoordinateType;
  AxisOrder := NewAxisOrder;
End;

Function TCoordinateSystem.AxisNames: TAxisNames;
Begin
  Case CoordinateType Of
  ctGeocentric:
    Result := GeocentricAxisNames;
  ctGeodetic:
    Result := GeodeticAxisNames;
  ctCartesian:
    Result := CartesianAxisNames;
  End;
End;

Function TCoordinateSystem.ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  { Actual conversions performed by child implementations. }
  Result := NullCoordinates;
End;

Function TCoordinateSystem.ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates;
Begin
  { Actual conversions performed by child implementations. }
  Result := NullCoordinates;
End;

Var
  CoordinateSystemsList: Array Of TCoordinateSystemPointer;

Function TCoordinateSystems.AvailableSystemsList(OmitIndex: Integer = -1): String;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Index<>OmitIndex Then
      Begin
        Result := Result+CoordinateSystemsList[Index]^.Description;
        If Index<>LastIndex Then
          Result := Result+LineEnding;
      End;
End;

Function TCoordinateSystems.FindByDescription(Const Description: String): Integer;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Description=CoordinateSystemsList[Index]^.Description Then
      Begin
        Result := Index;
        Exit;
      End;
  Result := -1;
End;

Function TCoordinateSystems.FindEPSGNumber(Const Number: Integer): Integer;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Number=CoordinateSystemsList[Index]^.EPSGNumber Then
      Begin
        Result := Index;
        Exit;
      End;
  Result := -1;
End;

Function TCoordinateSystems.IndexOf(Const CoordinateSystem: TCoordinateSystem): Integer;
Begin
  Result := FindEPSGNumber(CoordinateSystem.EPSGNumber);
End;

Function TCoordinateSystems.Items(Index: Integer): TCoordinateSystem;
Begin
  Result := CoordinateSystemsList[Index]^;
End;

Function TCoordinateSystems.Pointers(Index: Integer): TCoordinateSystemPointer;
Begin
  Result := CoordinateSystemsList[Index];
End;

Procedure TCoordinateSystems.Register(Const CoordinateSystem: TCoordinateSystem);
Begin
  Count := Length(CoordinateSystemsList);
  Inc(Count);
  SetLength(CoordinateSystemsList, Count);
  CoordinateSystemsList[High(CoordinateSystemsList)] := @CoordinateSystem;
End;

End.

