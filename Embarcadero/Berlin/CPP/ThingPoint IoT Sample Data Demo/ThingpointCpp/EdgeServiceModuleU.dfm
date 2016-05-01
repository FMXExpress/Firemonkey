object EdgeServiceModule: TEdgeServiceModule
  OldCreateOrder = False
  Height = 284
  Width = 463
  object EMSEdgeService1: TEMSEdgeService
    AutoActivate = False
    ModuleName = 'EdgeModule1'
    ModuleVersion = '1'
    Provider = EMSProvider1
    ListenerProtocol = 'http'
    ListenerService.Port = 8081
    ListenerService.Host = 'localhost'
    OnRegistering = EMSEdgeService1Registering
    OnRegistered = EMSEdgeService1Registered
    Left = 56
    Top = 40
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 184
    Top = 48
  end
end
