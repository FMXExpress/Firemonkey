object SampleAttributesDelphiResource1: TSampleAttributesDelphiResource1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 417
  Width = 440
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    Connected = True
    LoginPrompt = False
    Left = 74
    Top = 39
  end
  object EmployeeTable: TFDQuery
    Active = True
    Connection = EmployeeConnection
    SchemaAdapter = FDSchemaAdapterEmployees
    FetchOptions.AssignedValues = [evItems, evCache]
    ResourceOptions.AssignedValues = [rvStoreItems]
    SQL.Strings = (
      'SELECT *  FROM EMPLOYEE')
    Left = 74
    Top = 103
  end
  object FDSchemaAdapterEmployees: TFDSchemaAdapter
    ResourceOptions.AssignedValues = [rvStoreItems]
    ResourceOptions.StoreItems = [siData]
    Left = 80
    Top = 232
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 168
    Top = 312
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 55
    Top = 312
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 320
    Top = 312
  end
end
