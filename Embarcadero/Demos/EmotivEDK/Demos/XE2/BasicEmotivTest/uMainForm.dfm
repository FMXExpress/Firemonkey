object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 248
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbEDK: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 465
    Height = 238
    Align = alTop
    Caption = 'Emotiv EDK'
    TabOrder = 0
    object lblStatus: TLabel
      Left = 2
      Top = 223
      Width = 461
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Caption = 'lblStatus'
      ExplicitTop = 208
      ExplicitWidth = 41
    end
    object gbDeviceCount: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 455
      Height = 55
      Align = alTop
      Caption = 'Device Count'
      TabOrder = 0
      DesignSize = (
        455
        55)
      object btnDeviceCount: TButton
        Left = 16
        Top = 20
        Width = 105
        Height = 25
        Caption = 'Get Device Count'
        TabOrder = 0
        OnClick = btnDeviceCountClick
      end
      object edDeviceCount: TEdit
        Left = 127
        Top = 22
        Width = 318
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 1
        Text = '0'
      end
    end
    object sbMouse: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 79
      Width = 455
      Height = 122
      Align = alTop
      Caption = 'Mouse Control'
      TabOrder = 1
      ExplicitLeft = 7
      ExplicitTop = 167
      object Label1: TLabel
        Left = 12
        Top = 22
        Width = 57
        Height = 13
        Caption = 'Dampening:'
      end
      object Label2: TLabel
        Left = 143
        Top = 22
        Width = 202
        Height = 13
        Caption = 'Higher number = slower mouse movement'
      end
      object Label3: TLabel
        Left = 12
        Top = 50
        Width = 57
        Height = 13
        Caption = 'Min Delta X:'
      end
      object Label4: TLabel
        Left = 143
        Top = 50
        Width = 57
        Height = 13
        Caption = 'Min Delta Y:'
      end
      object seSensitivity: TSpinEdit
        Left = 75
        Top = 19
        Width = 62
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 2
        OnChange = seSensitivityChange
      end
      object cbAutoCenter: TCheckBox
        Left = 75
        Top = 94
        Width = 97
        Height = 17
        Caption = 'Auto Centering'
        TabOrder = 1
      end
      object cbActive: TCheckBox
        Left = 8
        Top = 94
        Width = 54
        Height = 17
        Caption = 'Active'
        TabOrder = 2
      end
      object seMinX: TSpinEdit
        Left = 75
        Top = 47
        Width = 62
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 5
      end
      object seMinY: TSpinEdit
        Left = 206
        Top = 47
        Width = 62
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 5
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 244
    Width = 471
    Height = 4
    Align = alClient
    Caption = ' '
    TabOrder = 1
    ExplicitTop = 224
    ExplicitHeight = 102
    object lblUpperFace: TLabel
      Left = 8
      Top = 16
      Width = 62
      Height = 13
      Caption = 'lblUpperFace'
    end
    object lblEye: TLabel
      Left = 8
      Top = 35
      Width = 62
      Height = 13
      Caption = 'lblUpperFace'
    end
    object lblLowerFace: TLabel
      Left = 8
      Top = 54
      Width = 62
      Height = 13
      Caption = 'lblUpperFace'
    end
    object lblContacts: TLabel
      Left = 8
      Top = 73
      Width = 53
      Height = 13
      Caption = 'lblContacts'
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 216
    Top = 160
  end
end
