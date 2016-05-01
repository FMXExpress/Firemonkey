object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 302
  Width = 526
  object KinveyProvider1: TKinveyProvider
    ApiVersion = '3'
    Left = 79
    Top = 24
  end
  object BackendStorage1: TBackendStorage
    Provider = KinveyProvider1
    Left = 208
    Top = 24
  end
end
