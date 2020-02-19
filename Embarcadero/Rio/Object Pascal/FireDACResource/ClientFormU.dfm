object ClientForm: TClientForm
  Left = 0
  Top = 0
  Caption = 'EMS FireDAC Client'
  ClientHeight = 463
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 635
    Height = 191
    Align = alClient
    DataSource = ClientModule.dsCustomers2
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 232
    Width = 635
    Height = 231
    Align = alBottom
    DataSource = ClientModule.dsOrders2
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Action = ActionGetTables
      TabOrder = 0
    end
    object Button2: TButton
      Left = 89
      Top = 10
      Width = 75
      Height = 25
      Action = ActionPostUpdates
      TabOrder = 1
    end
  end
  object EMSFireDACClient1: TEMSFireDACClient
    Resource = 'test'
    Provider = EMSProvider1
    SchemaAdapter = ClientModule.FDSchemaAdapter2
    Left = 112
    Top = 144
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = 'localhost'
    URLPort = 8080
    ProxyPort = 8888
    Left = 112
    Top = 48
  end
  object ActionList1: TActionList
    Left = 408
    Top = 8
    object ActionGetTables: TAction
      Caption = 'Get Tables'
      OnExecute = ActionGetTablesExecute
      OnUpdate = ActionGetTablesUpdate
    end
    object ActionPostUpdates: TAction
      Caption = 'Post Updates'
      OnExecute = ActionPostUpdatesExecute
      OnUpdate = ActionPostUpdatesUpdate
    end
  end
end
