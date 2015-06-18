object MainForm: TMainForm
  Left = 580
  Top = 102
  Width = 737
  Height = 592
  Caption = 'Leap Swipe demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 248
    Top = 18
    Width = 56
    Height = 23
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Friction'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object SGSongs: TStringGrid
    Left = 32
    Top = 56
    Width = 648
    Height = 484
    ColCount = 3
    DefaultColWidth = 200
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    ScrollBars = ssNone
    TabOrder = 0
    ColWidths = (
      214
      214
      216)
  end
  object MEFriction: TMaskEdit
    Left = 312
    Top = 18
    Width = 80
    Height = 23
    AutoSize = False
    EditMask = '000;1;_'
    MaxLength = 3
    TabOrder = 1
    Text = '10 '
  end
  object CBPercentual: TCheckBox
    Left = 400
    Top = 16
    Width = 81
    Height = 26
    Caption = 'Percentual'
    TabOrder = 2
  end
  object BStart: TButton
    Left = 24
    Top = 18
    Width = 123
    Height = 25
    Caption = 'St&art leap detection'
    TabOrder = 3
    OnClick = BStartClick
  end
  object Bstop: TButton
    Left = 160
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = BstopClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 416
    Top = 12
  end
end
