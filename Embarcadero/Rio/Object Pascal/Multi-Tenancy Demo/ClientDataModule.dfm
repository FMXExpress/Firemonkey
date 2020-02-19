object ClientDM: TClientDM
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Height = 318
  Width = 380
  object EMSProvider: TEMSProvider
    ApiVersion = '1'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 104
    Top = 14
  end
  object EMSFireDACClientItems: TEMSFireDACClient
    Resource = 'items'
    Provider = EMSProvider
    SchemaAdapter = FDSchemaAdapter
    Auth = BackendAuth
    Left = 48
    Top = 96
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 248
    Top = 8
  end
  object FDStanStorageJSONLink: TFDStanStorageJSONLink
    Left = 296
    Top = 8
  end
  object BackendEndpointSettings: TBackendEndpoint
    Provider = EMSProvider
    Params = <>
    Resource = 'settings'
    Response = RESTResponse
    Left = 176
    Top = 96
  end
  object RESTResponse: TRESTResponse
    Left = 176
    Top = 152
  end
  object memStores: TFDMemTable
    FieldDefs = <
      item
        Name = 'TENANTID'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'TENANTNAME'
        DataType = ftString
        Size = 50
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 176
    Top = 216
  end
  object FDTableAdapter: TFDTableAdapter
    SchemaAdapter = FDSchemaAdapter
    DatSTableName = 'QueryGetItems'
    Left = 48
    Top = 200
  end
  object FDSchemaAdapter: TFDSchemaAdapter
    Left = 48
    Top = 152
  end
  object memItems: TFDMemTable
    FieldDefs = <
      item
        Name = 'ITEMID'
        DataType = ftInteger
      end
      item
        Name = 'TENANTID'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'BARCODE'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ITEMNAME'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'DESCRIPTION'
        DataType = ftString
        Size = 64
      end
      item
        Name = 'QTY'
        DataType = ftInteger
      end>
    CachedUpdates = True
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Adapter = FDTableAdapter
    StoreDefs = True
    Left = 40
    Top = 256
  end
  object BackendAuth: TBackendAuth
    Provider = EMSProvider
    LoginPrompt = False
    UserDetails = <>
    DefaultAuthentication = Application
    Left = 296
    Top = 96
  end
  object BackendUsers: TBackendUsers
    Provider = EMSProvider
    Auth = BackendAuth
    Left = 296
    Top = 168
  end
  object memCurrentUser: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'UserName'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'UserId'
        DataType = ftString
        Size = 38
      end
      item
        Name = 'GroupName'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'HasWriteAccess'
        DataType = ftBoolean
      end
      item
        Name = 'DisplayUser'
        DataType = ftString
        Size = 20
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 288
    Top = 248
    Content = {
      414442530F00CF1DCC020000FF00010001FF02FF0304001C0000006D0065006D
      00430075007200720065006E007400550073006500720005000A000000540061
      0062006C006500060000000000070000080032000000090000FF0AFF0B040010
      00000055007300650072004E0061006D00650005001000000055007300650072
      004E0061006D0065000C00010000000E000D000F001400000010000111000112
      000113000114000115000116001000000055007300650072004E0061006D0065
      00170014000000FEFF0B04000C00000055007300650072004900640005000C00
      00005500730065007200490064000C00020000000E000D000F00260000001000
      0111000112000113000114000115000116000C00000055007300650072004900
      6400170026000000FEFF0B040012000000470072006F00750070004E0061006D
      006500050012000000470072006F00750070004E0061006D0065000C00030000
      000E000D000F0014000000100001110001120001130001140001150001160012
      000000470072006F00750070004E0061006D006500170014000000FEFF0B0400
      1C00000048006100730057007200690074006500410063006300650073007300
      05001C0000004800610073005700720069007400650041006300630065007300
      73000C00040000000E0018001000011100011200011300011400011500011600
      1C00000048006100730057007200690074006500410063006300650073007300
      FEFF0B04001600000044006900730070006C0061007900550073006500720005
      001600000044006900730070006C006100790055007300650072000E000D000F
      0014000000100001110001190001130001140001150001160016000000440069
      00730070006C00610079005500730065007200170014000000FEFEFF1AFEFF1B
      FEFF1CFF1D1E0000000000FF1F00000400000054657374FEFEFEFEFEFF20FEFF
      21220001000000FF23FEFEFE0E004D0061006E0061006700650072001E005500
      7000640061007400650073005200650067006900730074007200790012005400
      610062006C0065004C006900730074000A005400610062006C00650008004E00
      61006D006500140053006F0075007200630065004E0061006D0065000A005400
      6100620049004400240045006E0066006F0072006300650043006F006E007300
      74007200610069006E00740073001E004D0069006E0069006D0075006D004300
      6100700061006300690074007900180043006800650063006B004E006F007400
      4E0075006C006C00140043006F006C0075006D006E004C006900730074000C00
      43006F006C0075006D006E00100053006F007500720063006500490044001800
      6400740041006E007300690053007400720069006E0067001000440061007400
      610054007900700065000800530069007A006500140053006500610072006300
      6800610062006C006500120041006C006C006F0077004E0075006C006C000800
      420061007300650014004F0041006C006C006F0077004E0075006C006C001200
      4F0049006E0055007000640061007400650010004F0049006E00570068006500
      720065001A004F0072006900670069006E0043006F006C004E0061006D006500
      140053006F007500720063006500530069007A00650012006400740042006F00
      6F006C00650061006E001400430061006C00630075006C006100740065006400
      1C0043006F006E00730074007200610069006E0074004C006900730074001000
      56006900650077004C006900730074000E0052006F0077004C00690073007400
      060052006F0077000A0052006F0077004900440010004F007200690067006900
      6E0061006C001800520065006C006100740069006F006E004C00690073007400
      1C0055007000640061007400650073004A006F00750072006E0061006C001200
      530061007600650050006F0069006E0074000E004300680061006E0067006500
      7300}
    object memCurrentUserUserName: TStringField
      FieldName = 'UserName'
    end
    object memCurrentUserUserId: TStringField
      FieldName = 'UserId'
      Size = 38
    end
    object memCurrentUserGroupName: TStringField
      FieldName = 'GroupName'
    end
    object memCurrentUserHasWriteAccess: TBooleanField
      FieldName = 'HasWriteAccess'
    end
    object memCurrentUserDisplayUser: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'DisplayUser'
      OnGetText = memCurrentUserDisplayUserGetText
    end
  end
end
