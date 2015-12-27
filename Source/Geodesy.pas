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
  SysUtils, Math, Geometry;

Type
  TCoordinateType = (ctCartesian, ctGeodetic, ctProjected);

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
  TAxisNamesPointer = ^TAxisNames;

Type
  TPlanarCoordinates = Packed Object(Geometry.T2DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
  End;
  TPlanarCoordinatesPointer = ^TPlanarCoordinates;

Type
  TCoordinates = Packed Object(Geometry.T3DCoordinates)
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Latitude: TCoordinate Read Y Write Y;
    Property Longitude: TCoordinate Read X Write X;
    Property Altitude: TCoordinate Read Z Write Z;
  End;
  TCoordinatesPointer = ^TCoordinates;

Type
  TMeasuredCoordinates = Packed Object(Geometry.T3DCoordinates)
    M: TCoordinate;
    Property Easting: TCoordinate Read X Write X;
    Property Northing: TCoordinate Read Y Write y;
    Property Elevation: TCoordinate Read Z Write Z;
    Property Measure: TCoordinate Read M Write M;
  End;
 TMeasuredCoordinatesPointer = ^TMeasuredCoordinates;

Type
  TGeodeticBounds = Packed Object
    Western: TCoordinate;
    Southern: TCoordinate;
    Eastern: TCoordinate;
    Northern: TCoordinate;
  End;
  TGeodeticBoundsPointer = ^TGeodeticBounds;

Type
  TSexagesimalCoordinate = Packed Object
      Degrees, Minutes, Seconds, Sign: TCoordinate;
  End;
  TSexagesimalCoordinatePointer = ^TSexagesimalCoordinate;

Type
  TSexagesimalCoordinates = Packed Object
    Latitude, Longitude: TSexagesimalCoordinate;
    Altitude: TCoordinate;
  End;
  TSexagesimalCoordinatesPointer = ^TSexagesimalCoordinates;

Type
  THelmertTransformParameters = Packed Object
    Translation: TCoordinates;
    Rotation: TCoordinates;
    Scale: TCoordinate;
  End;
  THelmertTransformParametersPointer = ^THelmertTransformParameters;

Type
  TEllipsoid = Packed Object
    SemiMajorAxis: TCoordinate;
    SemiMinorAxis: TCoordinate;
    SemiMajorAxisSquared: TCoordinate;
    SemiMinorAxisSquared: TCoordinate;
    Eccentricity: TCoordinate;
    EccentricitySquared: TCoordinate;
    Constructor Initialize(NewSemiMajorAxis, NewSemiMinorAxis: TCoordinate);
  End;
  TEllipsoidPointer = ^TEllipsoid;

Type
  TProjection = Packed Object
    Ellipsoid: TEllipsoid;
    OriginOffset: TCoordinates;
    MeridianScaleFactor: TCoordinate;
    TrueOrigin: TCoordinates;
    Constructor Initialize(NewMeridianScaleFactor, NewTrueOriginLatitude, NewTrueOriginLongitude, NewOriginOffsetEasting, NewOriginOffsetNorthing: TCoordinate; NewEllipsoid: TEllipsoid);
  End;
  TProjectionPointer = ^TProjection;

Const
  OneOverSixty: TCoordinate = 1/60;
  OneOverSixtySquared: TCoordinate = 1/(60*60);
  NullCoordinates: TCoordinates = (X: 0; Y: 0; Z: 0);

{ Coordinates operator overloads. }
Operator = (A, B: TCoordinates): Boolean;
Operator := (A: TPlanarCoordinates): TCoordinates;
Operator + (A, B: TPlanarCoordinates): TPlanarCoordinates;
Operator + (A, B: TCoordinates): TCoordinates;
Operator + (A: TCoordinates; B: TPlanarCoordinates): TCoordinates;
Operator - (A, B: TPlanarCoordinates): TPlanarCoordinates;
Operator - (A, B: TCoordinates): TCoordinates;
Operator - (A: TCoordinates; B: TPlanarCoordinates): TCoordinates;

{ Geodesy functions. }
Function GeodeticDegToRad(Const Coordinates: TCoordinates): TCoordinates;
Function GeodeticRadToDeg(Const Coordinates: TCoordinates): TCoordinates;
Function NormalizeLatitude(Angle: TCoordinate): TCoordinate;
Function NormalizeLongitude(Angle: TCoordinate): TCoordinate;
Function AxisTypeFromIndex(Index: Integer; AxisOrder: TAxisOrder = aoXYZ): TAxisType;
Function SexagesimalToDecimalCoordinate(Const Coordinate: TSexagesimalCoordinate): TCoordinate;
Function DecimalToSexagesimalCoordinate(Const Coordinate: TCoordinate; IgnoreSeconds: Boolean = False): TSexagesimalCoordinate;
Function SexagesimalToDecimalCoordinates(Const Coordinates: TSexagesimalCoordinates): TCoordinates;
Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates; IgnoreSeconds: Boolean = False): TSexagesimalCoordinates;
Function GeodeticToGeocentric(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid):TCoordinates;
Function GeocentricToGeodetic(Const Coordinates: TCoordinates; Const Ellipsoid: TEllipsoid):TCoordinates;
Function HelmertTransform(Const Coordinates: TCoordinates; Const HelmertTransformParameters: THelmertTransformParameters): TCoordinates;
Function TransverseMercator(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;
Function InverseTransverseMercator(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;
Function WithinGeodeticBounds(Coordinates: TCoordinates; Bounds: TGeodeticBounds): Boolean; {$IFDEF USE_INLINE}Inline;{$ENDIF}

Const
  GeocentricAxisNames: TAxisNames = (LongX: 'X Coordinate'; LongY: 'Y Coordinate'; LongZ: 'Z Coordinate';
                                     ShortX: 'X'; ShortY: 'Y'; ShortZ: 'Z');
  GeodeticAxisNames: TAxisNames = (LongX: 'Longitude'; LongY: 'Latitude'; LongZ: 'Altitude';
                                   ShortX: 'Long'; ShortY: 'Lat'; ShortZ: 'Alt');
  CartesianAxisNames: TAxisNames = (LongX: 'Easting'; LongY: 'Northing'; LongZ: 'Elevation';
                                    ShortX: 'East'; ShortY: 'North'; ShortZ: 'Height');

// TODO: Remove back to OSTab.
Type
  TVerticalDatumCode = (vdNone, vdOrdnanceDatumNewlyn, vdStMarys, vdDouglas02,
                        vdStornoway, vdStKilda, vdLerwick, vdNewlyn, vdFairIsle,
                        vdFlannanIsles, vdNorthRona, vdSuleSkerry, vdFoula,
                        vdMalinHead, vdBelfast, vdOffshore);

Type
  TCoordinateSystem = Object
    Abbreviation: String;
    AxisOrder: TAxisOrder;
    CoordinateType: TCoordinateType;
    Description: String;
    EPSGNumber: Integer;
    GeodeticBounds: TGeodeticBounds;
    Name: String;
    PreferredVerticalDatum: TVerticalDatumCode; { These fields should be in the TITMCoordinateSystem declaration in the ITM unit. }
    LastVerticalDatum: TVerticalDatumCode; { They have been declared here to avoid a compiler bug that incorrectly handles object inheritance. }
    Constructor Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                           NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder; NewBounds: TGeodeticBounds);
    Function AxisNames: TAxisNames;
    Function ConvertToGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual; Abstract;
    Function ConvertFromGeocentric(Coordinates: TCoordinates): TCoordinates; Virtual; Abstract;
    Function WithinGeodeticBounds(Coordinates: TCoordinates): Boolean;
  End;
  TCoordinateSystemPointer = ^TCoordinateSystem;

Type
  TCoordinateSystems = Object
    Count: Integer;
    Function AvailableSystemsList(EPSGPrefix: Boolean = False): String;
    Function CompatibleSystemsList(SystemIndex: Integer): String;
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

Operator=(A, B: TCoordinates): Boolean;
Begin
  Result := (T3DCoordinates(A)=T3DCoordinates(B));
End;

Operator:=(A: TPlanarCoordinates): TCoordinates;
Begin
  Result.Easting := A.Easting;
  Result.Northing := A.Northing;
  Result.Elevation := 0;
End;

Operator+(A, B: TPlanarCoordinates): TPlanarCoordinates;
Begin
  Result := TPlanarCoordinates(T2DCoordinates(A)+T2DCoordinates(B));
End;

Operator+(A, B: TCoordinates): TCoordinates;
Begin
  Result := TCoordinates(T3DCoordinates(A)+T3DCoordinates(B));
End;

Operator+(A: TCoordinates; B: TPlanarCoordinates): TCoordinates;
Begin
  Result.X := A.X+B.X;
  Result.Y := A.Y+B.Y;
  Result.Z := A.Z;
End;

Operator-(A, B: TPlanarCoordinates): TPlanarCoordinates;
Begin
  Result := TPlanarCoordinates(T2DCoordinates(A)-T2DCoordinates(B));
End;

Operator-(A, B: TCoordinates): TCoordinates;
Begin
  Result := TCoordinates(T3DCoordinates(A)-T3DCoordinates(B));
End;

Operator-(A: TCoordinates; B: TPlanarCoordinates): TCoordinates;
Begin
  Result.X := A.X-B.X;
  Result.Y := A.Y-B.Y;
  Result.Z := A.Z;
End;

Function GeodeticDegToRad(Const Coordinates: TCoordinates): TCoordinates;
Begin
  Result.Latitude := DegToRad(Coordinates.Latitude);
  Result.Longitude := DegToRad(Coordinates.Longitude);
  Result.Altitude := Coordinates.Altitude;
End;

Function GeodeticRadToDeg(Const Coordinates: TCoordinates): TCoordinates;
Begin
  Result.Latitude := RadToDeg(Coordinates.Latitude);
  Result.Longitude := RadToDeg(Coordinates.Longitude);
  Result.Altitude := Coordinates.Altitude;
End;

Function NormalizeLatitude(Angle: TCoordinate): TCoordinate;
Begin
  Result := Angle;
  While Result>HalfPi Do
    Result -= PI;
  While Result<-HalfPi Do
    Result += PI;
End;

Function NormalizeLongitude(Angle: TCoordinate): TCoordinate;
Begin
  Result := Angle;
  While Result>Pi Do
    Result -= TwoPI;
  While Result<-Pi Do
    Result += TwoPI;
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

Function DecimalToSexagesimalCoordinate(Const Coordinate: TCoordinate; IgnoreSeconds: Boolean = False): TSexagesimalCoordinate;
Var
  Value: TCoordinate;
Const
  Epsilon: TCoordinate = 1E-10; { Threshold for PLOS error compensation. }
Begin
  With Result Do
    Begin
      Sign := Math.Sign(Coordinate);
      Value := Abs(Coordinate);
      Degrees := Int(Value+Epsilon);
      Value := (Value-Degrees)*60;
      If Value<0 Then
        Begin
          Minutes := 0;
          Seconds := 0;
        End
      Else
        If IgnoreSeconds Then
          Begin
            Minutes := Value;
            Seconds := 0;
          End
        Else
          Begin
            Minutes := Int(Value+Epsilon);
            Value := (Value-Minutes)*60;
            If Value<0 Then
              Seconds := 0
            Else
              Seconds := Value;
          End;
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

Function DecimalToSexagesimalCoordinates(Const Coordinates: TCoordinates; IgnoreSeconds: Boolean = False): TSexagesimalCoordinates;
Begin
  With Result Do
    Begin
      Latitude := DecimalToSexagesimalCoordinate(Coordinates.Latitude, IgnoreSeconds);
      Longitude := DecimalToSexagesimalCoordinate(Coordinates.Longitude, IgnoreSeconds);
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
      { Test for potential division by zero and substitute the limiting value of arctan. }
      If Abs(X)<Epsilon Then
        Result.Longitude := Sign(Y)*PI/2
      Else
        Result.Longitude := ArcTan(Y/X);
      Param := Sqrt(X*X+Y*Y);
      If Param<Epsilon Then
        Begin
          Result.Latitude := Sign(Z)*PI/2;
          Result.Altitude := Z-SemiMinorAxis;
        End
      Else
        Begin
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

Function TransverseMercator(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;
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
  EtaSquared: TCoordinate;
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
  With Projection, Projection.Ellipsoid Do
    Begin
      SinLatitude := Sin(Coordinates.Latitude);
      SinLatitudePow2 := SinLatitude*SinLatitude;
      CosLatitude := Cos(Coordinates.Latitude);
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
      EtaSquared := (Nu/rho)-1.0;
      LatitudeDelta := Coordinates.Latitude-TrueOrigin.Latitude;
      LatitudeSum := Coordinates.Latitude+TrueOrigin.Latitude;
      Term1 := (1.0+n+5.0/4.0*(n2+n3))*LatitudeDelta;// TODO: Possible removal of divisions here?
      Term2 := (3.0*(n+n2)+21.0/8.0*n3)*Sin(LatitudeDelta)*Cos(LatitudeSum);
      Term3 := 15.0/8.0*(n2+n3)*Sin(2.0*LatitudeDelta)*Cos(2.0*LatitudeSum);
      Term4 := 35.0/24.0*n3*Sin(3.0*LatitudeDelta)*Cos(3.0*LatitudeSum);
      M := SemiMinorAxis*MeridianScaleFactor*(Term1-Term2+Term3-Term4);
      I := M+OriginOffset.Northing;
      II := (Nu/2.0)*SinLatitude*CosLatitude;
      III := (Nu/24.0)*SinLatitude*CosLatitudePow3*(5.0-TanLatitudePow2+9.0*EtaSquared);
      IIIA := (Nu/720.0)*SinLatitude*CosLatitudePow5*(61.0-58.0*TanLatitudePow2+TanLatitudePow4);
      IV := Nu*CosLatitude;
      V := (Nu/6.0)*CosLatitudePow3*(Nu/Rho-TanLatitudePow2);
      VI := (Nu/120.0)*CosLatitudePow5*(5.0-18.0*TanLatitudePow2+TanLatitudePow4+14.0*EtaSquared-58.0*TanLatitudePow2*EtaSquared);
      L := Coordinates.Longitude-TrueOrigin.Longitude;
      L2 := L*L;
      L3 := L2*L;
      L4 := L3*L;
      L5 := L4*L;
      L6 := L5*L;
      Result.Northing := I+II*L2+III*L4+IIIA*L6;
      Result.Easting := OriginOffset.Easting+IV*L+V*L3+VI*L5;
      Result.Elevation := Coordinates.Altitude; { Preserve any altitude information. }
    End;
End;

Function MeridionalArcLength(ScaledMajorAxis, ScaledMinorAxis, CentralLatitude, Latitude: TCoordinate): TCoordinate; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
  LatitudeDelta: TCoordinate;
  LatitudeSum: TCoordinate;
  n, n2, n3: TCoordinate;
  Term1, Term2, Term3, Term4: TCoordinate;
Begin
  n := (ScaledMajorAxis-ScaledMinorAxis)/(ScaledMajorAxis+ScaledMinorAxis);
  n2 := n*n;
  n3 := n2*n;
  LatitudeDelta := Latitude-CentralLatitude;
  LatitudeSum := Latitude+CentralLatitude;
  Term1 := (1+n+5/4*(n2+n3))*LatitudeDelta;
  Term2 := (3*(n+n2)+21/8*n3)*Sin(LatitudeDelta)*Cos(LatitudeSum);
  Term3 := 15/8*(n2+n3)*Sin(2*LatitudeDelta)*Cos(2*LatitudeSum);
  Term4 := 35/24*n3*Sin(3*LatitudeDelta)*Cos(3*LatitudeSum);
  Result := ScaledMinorAxis*(Term1-Term2+Term3-Term4);
End;

Function CentralMeridianLatitude(ScaledMajorAxis, ScaledMinorAxis, Northing, CentralNorthing, CentralLatitude: TCoordinate): TCoordinate; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
  Latitude: TCoordinate;
  PriorLatitude: TCoordinate;
  ArcLength: TCoordinate;
  NorthDelta: TCoordinate;
  Iteration: Integer;
Const
  IterationLimit = 6;
  Epsilon = 0.00001;
Begin
  PriorLatitude := CentralLatitude;
  ArcLength := 0;
  NorthDelta := Northing-CentralNorthing;
  For Iteration := 1 To IterationLimit Do
    Begin
      Latitude := ((NorthDelta-ArcLength)/ScaledMajorAxis)+PriorLatitude;
      ArcLength := MeridionalArcLength(ScaledMajorAxis, ScaledMinorAxis, CentralLatitude, Latitude);
      If Abs(NorthDelta-ArcLength)<Epsilon Then
        Break;
      PriorLatitude := Latitude;
    End;
  Result := Latitude;
End;

Function InverseTransverseMercator(Const Coordinates: TCoordinates; Const Projection: TProjection): TCoordinates;
Var
  ScaledMajorAxis: TCoordinate;
  ScaledMinorAxis: TCoordinate;
  InitialLatitude: TCoordinate;
  SinLatitude: TCoordinate;
  SinLatitude2: TCoordinate;
  Nu, Rho, EtaSquared: TCoordinate;
  TanLat, TanLat2, TanLat4, TanLat6: TCoordinate;
  SubExpression: TCoordinate;
  Nu2, Nu3, Nu5, Nu7: TCoordinate;
  VII, VIII, IX, X, XI, XII, XIIA: TCoordinate;
  SecLatitude: TCoordinate;
  EastDelta, EastDelta2, EastDelta3, EastDelta4, EastDelta5, EastDelta6, EastDelta7: TCoordinate;
Begin
  With Projection, Projection.Ellipsoid Do
    Begin
      ScaledMajorAxis := SemiMajorAxis*MeridianScaleFactor;
      ScaledMinorAxis := SemiMinorAxis*MeridianScaleFactor;
      InitialLatitude := CentralMeridianLatitude(ScaledMajorAxis, ScaledMinorAxis, Coordinates.Northing, OriginOffset.Northing, TrueOrigin.Latitude);
      SinLatitude := Sin(InitialLatitude);
      SinLatitude2 := SinLatitude*SinLatitude;
      SubExpression := 1-EccentricitySquared*SinLatitude2;
      Nu := ScaledMajorAxis*Power(SubExpression, -0.5);
      Rho := ScaledMajorAxis*(1-EccentricitySquared)*Power(SubExpression, -1.5);
      EtaSquared := (Nu/Rho)-1;
      TanLat := Tan(InitialLatitude);
      TanLat2 := TanLat*TanLat;
      TanLat4 := TanLat2*TanLat2;
      TanLat6 := TanLat4*TanLat2;
      Nu2 := Nu*Nu;
      Nu3 := Nu2*Nu;
      Nu5 := Nu3*Nu2;
      Nu7 := Nu5*Nu2;
      VII := TanLat/(2*Rho*Nu);
      VIII := (TanLat/(24*Rho*Nu3))*(5+3*TanLat2+EtaSquared-9*TanLat2*EtaSquared);
      IX := (TanLat/(720*Rho*Nu5))*(61+90*TanLat2+45*TanLat4);
      SecLatitude := Sec(InitialLatitude);
      X := SecLatitude/Nu;
      XI := SecLatitude/(6*Nu3)*(Nu/Rho+2*TanLat2);
      XII := SecLatitude/(120*Nu5)*(5+28*TanLat2+24*TanLat4);
      XIIA := SecLatitude/(5040*Nu7)*(61+662*TanLat2+1320*TanLat4+720*TanLat6);
      EastDelta := (Coordinates.Easting-OriginOffset.Easting);
      EastDelta2 := EastDelta*EastDelta;
      EastDelta3 := EastDelta2*EastDelta;
      EastDelta4 := EastDelta3*EastDelta;
      EastDelta5 := EastDelta4*EastDelta;
      EastDelta6 := EastDelta5*EastDelta;
      EastDelta7 := EastDelta6*EastDelta;
      Result.Latitude := InitialLatitude-VII*EastDelta2+VIII*EastDelta4-IX*EastDelta6;
      Result.Longitude := TrueOrigin.Longitude+X*EastDelta-XI*EastDelta3+XII*EastDelta5-XIIA*EastDelta7;
      Result.Altitude := Coordinates.Elevation;
    End;
End;

Function WithinGeodeticBounds(Coordinates: TCoordinates; Bounds: TGeodeticBounds): Boolean; {$IFDEF USE_INLINE}Inline;{$ENDIF}
Begin
  If Coordinates.Longitude<Bounds.Eastern Then
    If Coordinates.Latitude<Bounds.Northern Then
      If Coordinates.Longitude>=Bounds.Western Then
        If Coordinates.Latitude>=Bounds.Southern Then
              Begin
                Result := True;
                Exit;
              End;
  Result := False;
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

Constructor TCoordinateSystem.Initialize(NewName: String; NewAbbreviation: String; NewDescription: String; NewEPSGNumber: Integer;
                                         NewCoordinateType: TCoordinateType; NewAxisOrder: TAxisOrder; NewBounds: TGeodeticBounds);
Begin
  Name := NewName;
  Abbreviation := NewAbbreviation;
  Description := NewDescription;
  EPSGNumber := NewEPSGNumber;
  CoordinateType := NewCoordinateType;
  AxisOrder := NewAxisOrder;
  GeodeticBounds := NewBounds;
End;

Function TCoordinateSystem.AxisNames: TAxisNames;
Begin
  Case CoordinateType Of
  ctCartesian:
    Result := GeocentricAxisNames;
  ctGeodetic:
    Result := GeodeticAxisNames;
  ctProjected:
    Result := CartesianAxisNames;
  End;
End;

Function TCoordinateSystem.WithinGeodeticBounds(Coordinates: TCoordinates): Boolean;
Begin
  Result := Geodesy.WithinGeodeticBounds(Coordinates, GeodeticBounds);
End;

Var
  CoordinateSystemsList: Array Of TCoordinateSystemPointer;

Type
  TIndexList = Set Of Byte;

Function BuildCoordinateSystemsList(OmitList: TIndexList; EPSGPrefix: Boolean = False): String;
Var
  FirstIndex: Integer;
  LastIndex: Integer;
  Index: Integer;
Begin
  Result := '';
  FirstIndex := Low(CoordinateSystemsList);
  LastIndex := High(CoordinateSystemsList);
  For Index := FirstIndex To LastIndex Do
    If Not (Index In OmitList) Then
      Begin
        If EPSGPrefix Then
          Result := Result+IntToStr(CoordinateSystemsList[Index]^.EPSGNumber)+' - ';
        Result := Result+CoordinateSystemsList[Index]^.Description;
        If Index<>LastIndex Then
          Result := Result+LineEnding;
      End;
End;


Function TCoordinateSystems.AvailableSystemsList(EPSGPrefix: Boolean = False): String;
Begin
  Result := BuildCoordinateSystemsList([], EPSGPrefix);
End;

Function TCoordinateSystems.CompatibleSystemsList(SystemIndex: Integer): String;
Var
  EPSGNumber: Integer;
Begin
  If SystemIndex=-1 Then
    EPSGNumber := 0
  Else
    EPSGNumber := CoordinateSystemsList[SystemIndex]^.EPSGNumber;
  With CoordinateSystems Do
    Case EPSGNumber Of
    0: { Full List }
      Result := BuildCoordinateSystemsList([]);
    25831:{ UTM Zone 31N, no Ireland overlap. }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex), Byte(FindEPSGNumber(25830)), Byte(FindEPSGNumber(25829)), Byte(FindEPSGNumber(29903)), Byte(FindEPSGNumber(2157))]);
    25830:{ UTM Zone 30N }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex), Byte(FindEPSGNumber(25831)), Byte(FindEPSGNumber(25829))]);
    25829:{ UTM Zone 29N }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex), Byte(FindEPSGNumber(25831)), Byte(FindEPSGNumber(25830))]);
    29903, 2157: { Irish Grid and Irish Transverse Mercator }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex), Byte(FindEPSGNumber(27700)), Byte(FindEPSGNumber(27701))]);
    27700, 27701: { British National Grid (TN02/GM02) and (TN15/GM15) }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex), Byte(FindEPSGNumber(29903)), Byte(FindEPSGNumber(2157))]);
    Else { Otherwsie build a full list of available systems omiting the source. }
      Result := BuildCoordinateSystemsList([Byte(SystemIndex)]);
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
  If (Index>=Low(CoordinateSystemsList)) And (Index<=High(CoordinateSystemsList)) Then
    Result := CoordinateSystemsList[Index]
  Else
    Result := Nil;
End;

Procedure TCoordinateSystems.Register(Const CoordinateSystem: TCoordinateSystem);
Begin
  Count := Length(CoordinateSystemsList);
  Inc(Count);
  SetLength(CoordinateSystemsList, Count);
  CoordinateSystemsList[High(CoordinateSystemsList)] := @CoordinateSystem;
End;

End.

