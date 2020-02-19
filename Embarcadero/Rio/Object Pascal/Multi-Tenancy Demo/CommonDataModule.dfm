object CommonDM: TCommonDM
  OldCreateOrder = False
  Height = 168
  Width = 239
  object IBConnection: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'User_Name=sysdba'
      'Password=masterkey'
      'Database=Emsserver'
      'DriverID=IB')
    Left = 48
    Top = 54
  end
  object FDPhysIBDriverLink: TFDPhysIBDriverLink
    Left = 151
    Top = 8
  end
  object QuerySetupDatabase: TFDQuery
    Connection = IBConnection
    Transaction = FDTransaction
    Left = 128
    Top = 64
  end
  object FDTransaction: TFDTransaction
    Connection = IBConnection
    Left = 128
    Top = 112
  end
  object FDScript: TFDScript
    SQLScripts = <>
    Connection = IBConnection
    Transaction = FDTransaction
    Params = <>
    Macros = <>
    Left = 40
    Top = 112
  end
end
