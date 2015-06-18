object dtmdlMain: TdtmdlMain
  OldCreateOrder = False
  Height = 433
  Width = 852
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=C:\data\bible.ib'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    Connected = True
    LoginPrompt = False
    BeforeConnect = FDConnectionBeforeConnect
    Left = 104
    Top = 72
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 216
    Top = 72
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 336
    Top = 72
  end
  object qryBooks: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Active = True
    CachedUpdates = True
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode, evRowsetSize, evRecordCountMode]
    FetchOptions.Mode = fmAll
    FetchOptions.RowsetSize = -1
    SQL.Strings = (
      
        'Select B.BOOK_ID, B.TITLE, B.SHORT_NAME, B.NOTE, T.TITLE TESTIME' +
        'NT from books B '
      'inner join TESTAMENTS T on t.TESTAMENTS_ID = b.TESTAMENT_ID'
      'order by Book_id'
      '')
    Left = 104
    Top = 160
    object qryBooksBOOK_ID: TIntegerField
      FieldName = 'BOOK_ID'
      Origin = 'BOOK_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryBooksTITLE: TStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Required = True
      Size = 60
    end
    object qryBooksNOTE: TStringField
      FieldName = 'NOTE'
      Origin = 'NOTE'
      Size = 170
    end
    object qryBooksTESTIMENT: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'TESTIMENT'
      Origin = 'TITLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 15
    end
    object qryBooksSHORT_NAME: TStringField
      FieldName = 'SHORT_NAME'
      Origin = 'SHORT_NAME'
      Size = 60
    end
  end
  object qryChapters: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Active = True
    AfterScroll = qryChaptersAfterScroll
    CachedUpdates = True
    MasterSource = dsBooks
    MasterFields = 'BOOK_ID'
    DetailFields = 'BOOK_ID'
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode, evRowsetSize, evCache, evRecordCountMode]
    FetchOptions.Mode = fmAll
    FetchOptions.RowsetSize = -1
    FetchOptions.Cache = [fiBlobs, fiMeta]
    SQL.Strings = (
      'Select C.*, B.TITLE BOOK_TITLE from CHAPTERS C '
      'INNER JOIN BOOKS B On B.BOOK_ID = C.BOOK_ID'
      'where C.BOOK_ID = :BOOK_ID '
      'ORDER BY CHAPTER_NUMBER')
    Left = 200
    Top = 224
    ParamData = <
      item
        Name = 'BOOK_ID'
        DataType = ftInteger
        ParamType = ptInput
        Size = 4
        Value = 1
      end>
    object qryChaptersCHAPTER_ID: TIntegerField
      FieldName = 'CHAPTER_ID'
      Origin = 'CHAPTER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryChaptersBOOK_ID: TIntegerField
      FieldName = 'BOOK_ID'
      Origin = 'BOOK_ID'
      Required = True
    end
    object qryChaptersCHAPTER_NUMBER: TIntegerField
      FieldName = 'CHAPTER_NUMBER'
      Origin = 'CHAPTER_NUMBER'
      Required = True
    end
    object qryChaptersTITLE: TStringField
      FieldName = 'TITLE'
      Origin = 'TITLE'
      Size = 232
    end
    object qryChaptersBOOK_TITLE: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'BOOK_TITLE'
      Origin = 'TITLE'
      ProviderFlags = []
      ReadOnly = True
      Size = 60
    end
  end
  object dsBooks: TDataSource
    DataSet = qryBooks
    Left = 152
    Top = 192
  end
  object qryVerses: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Active = True
    CachedUpdates = True
    MasterSource = dsChapter
    MasterFields = 'CHAPTER_ID'
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode, evRowsetSize, evCache, evRecordCountMode]
    FetchOptions.Mode = fmAll
    FetchOptions.RowsetSize = -1
    FetchOptions.Cache = [fiBlobs, fiMeta]
    SQL.Strings = (
      
        'Select verse_number, verse_text from verses where chapter_id = :' +
        'Chapter_id'
      'order by verse_number')
    Left = 304
    Top = 296
    ParamData = <
      item
        Name = 'CHAPTER_ID'
        DataType = ftInteger
        ParamType = ptInput
        Size = 4
        Value = 67
      end>
    object qryVersesVERSE_NUMBER: TIntegerField
      FieldName = 'VERSE_NUMBER'
      Origin = 'VERSE_NUMBER'
      Required = True
    end
    object qryVersesVERSE_TEXT: TStringField
      FieldName = 'VERSE_TEXT'
      Origin = 'VERSE_TEXT'
      Required = True
      Size = 540
    end
  end
  object dsChapter: TDataSource
    DataSet = qryChapters
    Left = 256
    Top = 256
  end
end
