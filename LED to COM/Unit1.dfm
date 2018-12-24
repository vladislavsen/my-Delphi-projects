object Form1: TForm1
  Left = 545
  Top = 191
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
  object LabelRed: TLabel
    Left = 240
    Top = 40
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 8
    Height = 13
    Caption = 'R'
  end
  object Label3: TLabel
    Left = 16
    Top = 88
    Width = 8
    Height = 13
    Caption = 'G'
  end
  object Label4: TLabel
    Left = 16
    Top = 136
    Width = 7
    Height = 13
    Caption = 'B'
  end
  object LabelGreen: TLabel
    Left = 240
    Top = 88
    Width = 6
    Height = 13
    Caption = '0'
  end
  object LabelBlue: TLabel
    Left = 240
    Top = 136
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label1: TLabel
    Left = 99
    Top = 288
    Width = 3
    Height = 13
  end
  object RedLevel: TTrackBar
    Left = 40
    Top = 32
    Width = 185
    Height = 33
    Max = 250
    TabOrder = 0
    TickStyle = tsNone
    OnChange = RedLevelChange
  end
  object cbxPortList: TComboBox
    Left = 8
    Top = 288
    Width = 73
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'cbxPortList'
  end
  object GreenLevel: TTrackBar
    Left = 40
    Top = 80
    Width = 185
    Height = 33
    Max = 250
    TabOrder = 2
    TickStyle = tsNone
    OnChange = RedLevelChange
  end
  object BlueLevel: TTrackBar
    Left = 40
    Top = 128
    Width = 185
    Height = 33
    Max = 250
    TabOrder = 3
    TickStyle = tsNone
    OnChange = RedLevelChange
  end
  object Button1: TButton
    Left = 96
    Top = 232
    Width = 81
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1094#1074#1077#1090
    TabOrder = 4
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 48
    Top = 168
    Width = 177
    Height = 49
    Caption = 'Test Colour'
    TabOrder = 5
    OnClick = Panel1Click
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 216
    Top = 280
  end
end
