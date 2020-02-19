object StoreResource: TStoreResource
  OldCreateOrder = False
  Height = 214
  Width = 240
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 96
    Top = 8
  end
  object FDStanStorageJSONLink: TFDStanStorageJSONLink
    Left = 56
    Top = 8
  end
  object QueryGetItems: TFDQuery
    CachedUpdates = True
    Connection = CommonDM.IBConnection
    SchemaAdapter = FDSchemaAdapter
    UpdateOptions.UpdateTableName = 'STORE_ITEMS'
    SQL.Strings = (
      'SELECT * FROM STORE_ITEMS WHERE TenantId = :TenantId')
    Left = 96
    Top = 144
    ParamData = <
      item
        Name = 'TenantId'
        DataType = ftString
        ParamType = ptInput
      end>
  end
  object FDSchemaAdapter: TFDSchemaAdapter
    Left = 96
    Top = 80
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 136
    Top = 8
  end
end
