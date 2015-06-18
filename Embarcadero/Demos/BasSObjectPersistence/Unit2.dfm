object DataModule2: TDataModule2
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 171
  Width = 401
  object ParseProvider1: TParseProvider
    ApiVersion = '1'
    Left = 136
    Top = 64
  end
  object KinveyProvider1: TKinveyProvider
    ApiVersion = '3'
    Left = 248
    Top = 64
  end
end
