object NotesResource1: TNotesResource1
  OldCreateOrder = False
  Height = 160
  Width = 467
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=notes.sdb'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 40
    Top = 32
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    FetchOptions.AssignedValues = [evItems]
    FetchOptions.Items = [fiDetails, fiMeta]
    SQL.Strings = (
      'select * from notes')
    Left = 120
    Top = 32
  end
  object EMSDataSetResource1: TEMSDataSetResource
    AllowedActions = [List, Get, Post, Put, Delete]
    DataSet = FDQuery1
    KeyFields = 'id'
    ValueFields = 'id;title;content'
    PageSize = 2
    Left = 120
    Top = 88
  end
end
