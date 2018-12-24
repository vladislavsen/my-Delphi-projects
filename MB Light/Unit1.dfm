object Form1: TForm1
  Left = 517
  Top = 206
  Width = 286
  Height = 358
  Caption = 'RGB '#1082#1086#1085#1090#1088#1086#1083#1083#1077#1088
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 99
    Top = 288
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 20
    Top = 35
    Width = 111
    Height = 13
    Caption = #1064#1080#1088#1080#1085#1072' '#1079#1086#1085#1099' '#1079#1072#1093#1074#1072#1090#1072
  end
  object Label3: TLabel
    Left = 20
    Top = 67
    Width = 118
    Height = 13
    Caption = #1063#1091#1074#1089#1090#1074'. '#1082' '#1080#1079#1084#1077#1085'. '#1094#1074#1077#1090#1072
  end
  object Label4: TLabel
    Left = 20
    Top = 99
    Width = 116
    Height = 13
    Caption = #1063#1072#1089#1090#1086#1090#1072' '#1089#1095#1080#1090#1099#1074',  '#1084#1089#1077#1082
  end
  object cbxPortList: TComboBox
    Left = 8
    Top = 288
    Width = 73
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cbxPortList'
  end
  object Button1: TButton
    Left = 96
    Top = 232
    Width = 81
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1094#1074#1077#1090
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 48
    Top = 168
    Width = 177
    Height = 49
    Caption = 'Test Colour'
    TabOrder = 2
  end
  object edtHeightZone: TEdit
    Left = 142
    Top = 32
    Width = 81
    Height = 21
    TabOrder = 3
    Text = '200'
    OnChange = edtHeightZoneChange
  end
  object edtColorChangeAccur: TEdit
    Left = 142
    Top = 64
    Width = 81
    Height = 21
    TabOrder = 4
    Text = '0'
    OnChange = edtColorChangeAccurChange
  end
  object edtRereadDelay: TEdit
    Left = 142
    Top = 96
    Width = 81
    Height = 21
    TabOrder = 5
    Text = '300'
    OnChange = edtRereadDelayChange
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 224
    Top = 240
  end
end
