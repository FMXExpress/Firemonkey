object dtmdlData: TdtmdlData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 218
  Width = 320
  object cdsIconData: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'Category;Description'
    Params = <>
    AfterPost = cdsIconDataAfterPost
    Left = 48
    Top = 96
    object cdsIconDataID: TIntegerField
      FieldName = 'ID'
    end
    object cdsIconDataCategory: TStringField
      FieldName = 'Category'
      Size = 50
    end
    object cdsIconDataDescription: TStringField
      FieldName = 'Description'
      Size = 50
    end
    object cdsIconDataSearchTerms: TMemoField
      FieldName = 'SearchTerms'
      BlobType = ftMemo
    end
    object cdsIconDataIcon: TBlobField
      FieldName = 'Icon'
    end
  end
  object dsIconData: TDataSource
    DataSet = cdsIconData
    Left = 48
    Top = 40
  end
end
