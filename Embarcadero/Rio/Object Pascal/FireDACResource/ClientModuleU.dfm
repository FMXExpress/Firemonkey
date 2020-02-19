object ClientModule: TClientModule
  OldCreateOrder = False
  Height = 325
  Width = 322
  object dsOrders2: TDataSource
    DataSet = mtOrders
    Left = 192
    Top = 192
  end
  object mtCustomers: TFDMemTable
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    AutoCommitUpdates = False
    Adapter = taCustomers
    Left = 40
    Top = 80
  end
  object taCustomers: TFDTableAdapter
    SchemaAdapter = FDSchemaAdapter2
    DatSTableName = 'qCustomers'
    Left = 40
    Top = 136
  end
  object dsCustomers2: TDataSource
    DataSet = mtCustomers
    Left = 40
    Top = 192
  end
  object mtOrders: TFDMemTable
    CachedUpdates = True
    IndexFieldNames = 'CustomerID'
    MasterSource = dsCustomers2
    MasterFields = 'CustomerID'
    FetchOptions.AssignedValues = [evMode, evDetailCascade]
    FetchOptions.Mode = fmAll
    FetchOptions.DetailCascade = True
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    AutoCommitUpdates = False
    Adapter = taOrders
    Left = 192
    Top = 80
  end
  object FDSchemaAdapter2: TFDSchemaAdapter
    Left = 112
    Top = 24
  end
  object taOrders: TFDTableAdapter
    SchemaAdapter = FDSchemaAdapter2
    DatSTableName = 'qOrders'
    Left = 186
    Top = 136
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 240
    Top = 24
  end
end
