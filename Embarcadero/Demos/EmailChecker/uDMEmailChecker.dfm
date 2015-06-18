object DMEmailChecker: TDMEmailChecker
  OldCreateOrder = False
  Height = 240
  Width = 351
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = '127.0.0.1'
    URLPort = 8080
    Left = 48
    Top = 32
  end
  object BackendEndpoint1: TBackendEndpoint
    Provider = EMSProvider1
    Params = <
      item
        name = 'item'
      end>
    Resource = 'EmailChecker'
    ResourceSuffix = '{item}'
    Response = RESTResponse1
    Left = 144
    Top = 32
  end
  object RESTResponse1: TRESTResponse
    Left = 240
    Top = 32
  end
end
