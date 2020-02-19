object ResourceModule: TResourceModule
  OldCreateOrder = False
  Height = 341
  Width = 488
  object dsOrders: TDataSource
    DataSet = qOrders
    Left = 160
    Top = 208
  end
  object dsCustomers: TDataSource
    DataSet = qCustomers
    Left = 56
    Top = 208
  end
  object qOrders: TFDQuery
    CachedUpdates = True
    IndexFieldNames = 'CustomerID'
    MasterSource = dsCustomers
    MasterFields = 'CustomerID'
    Connection = FDConnection1
    SchemaAdapter = FDSchemaAdapter1
    FetchOptions.AssignedValues = [evDetailCascade]
    FetchOptions.DetailCascade = True
    SQL.Strings = (
      'select * from Orders')
    Left = 160
    Top = 152
  end
  object qCustomers: TFDQuery
    CachedUpdates = True
    Connection = FDConnection1
    SchemaAdapter = FDSchemaAdapter1
    SQL.Strings = (
      'select * from Customers')
    Left = 56
    Top = 152
  end
  object FDSchemaAdapter1: TFDSchemaAdapter
    Left = 112
    Top = 96
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 336
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 240
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 144
    Top = 24
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 56
    Top = 24
  end
  object EMSDataSetResource1: TEMSDataSetResource
    AllowedActions = [List, Post]
    DataSet = qCustomers
    Left = 304
    Top = 152
  end
end
