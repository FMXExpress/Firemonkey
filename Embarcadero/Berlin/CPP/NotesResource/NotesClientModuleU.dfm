object NotesClientModule: TNotesClientModule
  OldCreateOrder = False
  Height = 430
  Width = 496
  object BackendAuth1: TBackendAuth
    Provider = EMSProvider1
    LoginPrompt = False
    UserDetails = <>
    Left = 392
    Top = 264
  end
  object BackendEndpointDeleteNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmDELETE
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Notes'
    ResourceSuffix = '{item}'
    Left = 168
    Top = 280
  end
  object BackendEndpointUpdateNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPUT
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Notes'
    ResourceSuffix = '{item}'
    Left = 168
    Top = 208
  end
  object BackendEndpointGetNotes: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <>
    Resource = 'Notes'
    Left = 168
    Top = 128
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 40
    Top = 16
  end
  object BackendEndpointGetNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Notes'
    ResourceSuffix = '{item}'
    Left = 176
    Top = 40
  end
  object BackendEndpointAddNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPOST
    Params = <>
    Resource = 'Notes'
    Left = 168
    Top = 344
  end
end
