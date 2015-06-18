object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'KinectSDKTest'
  ClientHeight = 308
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object rightPanel: TPanel
    Left = 144
    Top = 184
    Width = 10
    Height = 10
    BevelOuter = bvNone
    Color = clGreen
    ParentBackground = False
    TabOrder = 0
    Visible = False
  end
  object leftPanel: TPanel
    Left = 184
    Top = 144
    Width = 10
    Height = 10
    BevelOuter = bvNone
    Color = clRed
    ParentBackground = False
    TabOrder = 1
    Visible = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object ckDepth: TCheckBox
      Left = 0
      Top = 1
      Width = 50
      Height = 17
      Caption = 'Depth'
      TabOrder = 0
    end
    object ckHands: TCheckBox
      Left = 135
      Top = 1
      Width = 50
      Height = 17
      Caption = 'Hands'
      TabOrder = 1
    end
    object ckPlayer: TCheckBox
      Left = 64
      Top = 1
      Width = 50
      Height = 17
      Caption = 'Player'
      TabOrder = 2
    end
  end
end
