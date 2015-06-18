object dataMain: TdataMain
  OldCreateOrder = False
  Height = 306
  Width = 526
  object pbsPlayer: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Name'
        Generator = 'ContactNames'
        ReadOnly = False
      end
      item
        Name = 'Score'
        FieldType = ftUInteger
        Generator = 'Colors'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = pbsPlayerCreateAdapter
    Left = 64
    Top = 56
  end
  object pbsScoreboard: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Name'
        Generator = 'ContactNames'
        ReadOnly = False
      end
      item
        Name = 'Score'
        FieldType = ftUInteger
        Generator = 'Colors'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = pbsScoreboardCreateAdapter
    Left = 224
    Top = 56
  end
  object TetheringManager1: TTetheringManager
    OnRequestManagerPassword = TetheringManager1RequestManagerPassword
    OnRemoteManagerShutdown = TetheringManager1RemoteManagerShutdown
    OnEndAutoConnect = TetheringManager1EndAutoConnect
    Password = 'ClientPassword'
    Text = 'TetheringManagerClient'
    Left = 128
    Top = 128
  end
  object TetheringAppProfile1: TTetheringAppProfile
    Manager = TetheringManager1
    Text = 'TetheringAppProfile1'
    Group = 'Game'
    Actions = <
      item
        Name = 'actGetScores'
        IsPublic = True
        Kind = Mirror
        Action = actGetScores
        NotifyUpdates = False
      end>
    Resources = <
      item
        Name = 'Scoreboard'
        IsPublic = True
        Kind = Mirror
        OnResourceReceived = TetheringAppProfile1ResourceReceived
      end>
    Left = 128
    Top = 200
  end
  object pbsStatus: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Status'
        Generator = 'BitmapNames'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = pbsStatusCreateAdapter
    Left = 360
    Top = 56
  end
  object ActionList1: TActionList
    Left = 256
    Top = 152
    object actGetScores: TAction
      Text = 'actGetScores'
    end
  end
end
