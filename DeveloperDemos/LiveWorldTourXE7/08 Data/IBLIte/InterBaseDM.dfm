object dmInterBase: TdmInterBase
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 227
  Width = 406
  object IBLiteDB: TFDConnection
    Params.Strings = (
      'OpenMode=OpenOrCreate'
      'PageSize=4096'
      'Database=C:\Data\BIBLE.IB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=IBLite')
    ConnectedStoredUsage = [auDesignTime]
    AfterConnect = IBLiteDBAfterConnect
    Left = 72
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 280
    Top = 32
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Lite = True
    Left = 168
    Top = 32
  end
end
