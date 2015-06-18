object Form1: TForm1
  Left = 509
  Top = 85
  Caption = 'Form1'
  ClientHeight = 545
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    810
    545)
  PixelsPerInch = 96
  TextHeight = 13
  object Frame: TLabel
    Left = 200
    Top = 8
    Width = 29
    Height = 24
    AutoSize = False
    Caption = 'Frame'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object LFrameCount: TLabel
    Left = 240
    Top = 8
    Width = 6
    Height = 24
    AutoSize = False
    Caption = '?'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label1: TLabel
    Left = 304
    Top = 8
    Width = 78
    Height = 24
    AutoSize = False
    Caption = 'protocol  version'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object LVersion: TLabel
    Left = 392
    Top = 8
    Width = 6
    Height = 24
    AutoSize = False
    Caption = '?'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 424
    Top = 8
    Width = 23
    Height = 24
    AutoSize = False
    Caption = 'FPS:'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object LFPS: TLabel
    Left = 456
    Top = 8
    Width = 6
    Height = 24
    AutoSize = False
    Caption = '?'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object BStart: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = BStartClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 48
    Width = 783
    Height = 80
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object BStop: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = BStopClick
  end
  object Panel1: TPanel
    Left = 16
    Top = 143
    Width = 786
    Height = 394
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' '
    TabOrder = 3
    object PB: TPaintBox
      Left = 1
      Top = 1
      Width = 784
      Height = 392
      Align = alClient
      Color = clYellow
      ParentColor = False
      OnPaint = PBPaint
      ExplicitLeft = 16
      ExplicitTop = 143
      ExplicitWidth = 781
      ExplicitHeight = 390
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 504
    Top = 8
  end
end
