object EMSClientDataModule: TEMSClientDataModule
  OldCreateOrder = False
  Height = 303
  Width = 418
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 56
    Top = 32
  end
  object BackendEndpointMeasurements: TBackendEndpoint
    Provider = EMSProvider1
    Params = <>
    Resource = 'measurements'
    Left = 200
    Top = 24
  end
  object BackendEndpointEdgeMeasurements: TBackendEndpoint
    Provider = EMSProvider1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'module'
        Options = [poAutoCreated]
      end>
    Resource = 'edgemodules'
    ResourceSuffix = '{module}/measurements'
    Left = 88
    Top = 104
  end
  object BackendQueryEdgeModules: TBackendQuery
    Provider = EMSProvider1
    BackendService = 'Edgeresources'
    QueryLines.Strings = (
      'where={"resourcename":"Measurements"}')
    Left = 64
    Top = 216
  end
  object BackenEndpointEdgeDetailedMeasurements: TBackendEndpoint
    Provider = EMSProvider1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'module'
        Options = [poAutoCreated]
      end>
    Resource = 'edgemodules'
    ResourceSuffix = '{module}/measurements/detailed'
    Left = 224
    Top = 168
  end
end
