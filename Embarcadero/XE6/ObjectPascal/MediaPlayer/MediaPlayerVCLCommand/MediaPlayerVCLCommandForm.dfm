object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'VCL MediaPlayer Command'
  ClientHeight = 457
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 329
    Height = 25
    Align = alTop
    Caption = 'Available Players:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 186
  end
  object Label2: TLabel
    Left = 232
    Top = 356
    Width = 54
    Height = 19
    Caption = 'Volume'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LbPlayers: TListBox
    Left = 0
    Top = 25
    Width = 329
    Height = 80
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 19
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 48
    Top = 248
    Width = 105
    Height = 49
    Caption = 'Play/Pause'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 112
    Top = 144
    Width = 105
    Height = 49
    Caption = 'Find Players'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object VolumeTrack: TTrackBar
    Left = 248
    Top = 200
    Width = 45
    Height = 150
    Max = 100
    Orientation = trVertical
    TabOrder = 3
    OnChange = VolumeTrackChange
  end
  object VCLCommandManager: TTetheringManager
    OnEndManagersDiscovery = VCLCommandManagerEndManagersDiscovery
    OnEndProfilesDiscovery = VCLCommandManagerEndProfilesDiscovery
    OnRequestManagerPassword = VCLCommandManagerRequestManagerPassword
    Password = '1234'
    Text = 'VCLCommandManager'
    Left = 48
    Top = 392
  end
  object VCLCommandApp: TTetheringAppProfile
    Manager = VCLCommandManager
    Text = 'VCLCommandApp'
    Actions = <>
    Resources = <>
    Left = 232
    Top = 392
  end
end
