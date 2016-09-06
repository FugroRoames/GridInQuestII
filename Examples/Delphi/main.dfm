object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Convert Coords'
  ClientHeight = 201
  ClientWidth = 674
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 24
    Width = 51
    Height = 13
    Caption = 'Longtitude'
  end
  object Label2: TLabel
    Left = 256
    Top = 64
    Width = 39
    Height = 13
    Caption = 'Latitude'
  end
  object Label3: TLabel
    Left = 32
    Top = 24
    Width = 31
    Height = 13
    Caption = 'Datum'
  end
  object Label4: TLabel
    Left = 32
    Top = 64
    Width = 60
    Height = 13
    Caption = 'SRID Source'
  end
  object Label5: TLabel
    Left = 32
    Top = 105
    Width = 59
    Height = 13
    Caption = 'SRID Target'
  end
  object Label6: TLabel
    Left = 256
    Top = 105
    Width = 37
    Height = 13
    Caption = 'Altitude'
  end
  object Label7: TLabel
    Left = 528
    Top = 105
    Width = 46
    Height = 13
    Caption = 'Position Z'
  end
  object Label8: TLabel
    Left = 528
    Top = 64
    Width = 46
    Height = 13
    Caption = 'Position Y'
  end
  object Label9: TLabel
    Left = 528
    Top = 24
    Width = 46
    Height = 13
    Caption = 'Position X'
  end
  object edLong: TEdit
    Left = 256
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '-7'
  end
  object edLat: TEdit
    Left = 256
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '53'
  end
  object cbDatum: TComboBox
    Left = 32
    Top = 40
    Width = 200
    Height = 21
    TabOrder = 2
    Text = '13: Malin Head'
    Items.Strings = (
      '0: None'
      '1: Ordnance Datum Newlyn'
      '2: St. Marys'
      '3: Douglas02'
      '4: Stornoway'
      '5: St. Kilda'
      '6: Lerwick'
      '7: Newlyn'
      '8: FairIsle'
      '9: FlannanIsles'
      '10: North Rona'
      '11: Sule Skerry'
      '12: Foula'
      '13: Malin Head'
      '14: Belfast'
      '15: Offshore')
  end
  object cbSRIDSource: TComboBox
    Left = 32
    Top = 80
    Width = 200
    Height = 21
    ItemIndex = 1
    TabOrder = 3
    Text = '4937   ETRS89 Geodetic'
    Items.Strings = (
      '4936   ETRS89 Cartesian'
      '4937   ETRS89 Geodetic'
      '25829 ETRS89 / UTM Zone 29N'
      '25830 ETRS89 / UTM Zone 30N'
      '25831 ETRS89 / UTM Zone 31N'
      '27700 OSGB36 / British National Grid'
      '2157   Irish Transverse Mercator'
      '29903 Irish Grid')
  end
  object cbSRIDTarget: TComboBox
    Left = 32
    Top = 121
    Width = 200
    Height = 21
    ItemIndex = 6
    TabOrder = 4
    Text = '2157   Irish Transverse Mercator'
    Items.Strings = (
      '4936   ETRS89 Cartesian'
      '4937   ETRS89 Geodetic'
      '25829 ETRS89 / UTM Zone 29N'
      '25830 ETRS89 / UTM Zone 30N'
      '25831 ETRS89 / UTM Zone 31N'
      '27700 OSGB36 / British National Grid'
      '2157   Irish Transverse Mercator'
      '29903 Irish Grid')
  end
  object edTargetX: TEdit
    Left = 528
    Top = 40
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object edTargetY: TEdit
    Left = 528
    Top = 80
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object edAlt: TEdit
    Left = 256
    Top = 121
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '100'
  end
  object edTargetZ: TEdit
    Left = 528
    Top = 121
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 8
  end
  object Button1: TButton
    Left = 416
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 9
    OnClick = Button1Click
  end
  object FileListBox1: TFileListBox
    Left = 377
    Top = 109
    Width = 145
    Height = 97
    ItemHeight = 13
    Mask = '*.DAT'
    MultiSelect = True
    TabOrder = 10
    Visible = False
  end
  object Button2: TButton
    Left = 32
    Top = 160
    Width = 89
    Height = 25
    Caption = 'Irish Example'
    TabOrder = 11
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 143
    Top = 160
    Width = 89
    Height = 25
    Caption = 'UK Example'
    TabOrder = 12
    OnClick = Button3Click
  end
end
