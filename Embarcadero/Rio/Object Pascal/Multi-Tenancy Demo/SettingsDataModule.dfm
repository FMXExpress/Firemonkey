object SettingsResource: TSettingsResource
  OldCreateOrder = False
  Height = 150
  Width = 215
  object QueryGetActiveStores: TFDQuery
    Connection = CommonDM.IBConnection
    SQL.Strings = (
      
        'SELECT TENANTID, TENANTNAME FROM TENANTS WHERE ISACTIVE = 1 And ' +
        'TENANTID <> '#39'00000000-0000-0000-0000-000000000001'#39)
    Left = 88
    Top = 48
  end
end
