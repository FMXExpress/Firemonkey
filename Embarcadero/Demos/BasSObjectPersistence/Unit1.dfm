object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 282
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 660
    Height = 282
    ActivePage = tabFetch
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 105
    object tabSettings: TTabSheet
      Caption = 'Settings'
      ExplicitHeight = 77
      object Label3: TLabel
        Left = 16
        Top = 13
        Width = 38
        Height = 13
        Caption = 'Storage'
      end
      object EditBackendClassName: TLabeledEdit
        Left = 167
        Top = 32
        Width = 121
        Height = 21
        EditLabel.Width = 55
        EditLabel.Height = 13
        EditLabel.Caption = 'Class Name'
        TabOrder = 0
        Text = 'Customer'
      end
      object ComboBoxBackendService: TComboBox
        Left = 16
        Top = 32
        Width = 145
        Height = 21
        ItemIndex = 0
        TabOrder = 1
        Text = 'Storage'
        Items.Strings = (
          'Storage'
          'User'
          'Installation')
      end
    end
    object tabFetch: TTabSheet
      Caption = 'Fetch'
      ExplicitHeight = 77
      object Label2: TLabel
        Left = 0
        Top = 97
        Width = 652
        Height = 13
        Align = alTop
        Caption = 'Results'
        ExplicitTop = 128
      end
      object ListView1: TListView
        Left = 288
        Top = 110
        Width = 364
        Height = 144
        Align = alRight
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Phone'
            Width = 100
          end
          item
            Caption = 'ID'
            Width = 100
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 652
        Height = 97
        Align = alTop
        Caption = 'Panel1'
        TabOrder = 1
        object Label1: TLabel
          Left = 1
          Top = 1
          Width = 650
          Height = 13
          Align = alTop
          Caption = 'Query'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 30
        end
        object Button1: TButton
          Left = 576
          Top = 14
          Width = 75
          Height = 82
          Align = alRight
          Caption = 'Fetch'
          TabOrder = 0
          OnClick = Button1Click
          ExplicitLeft = 245
          ExplicitTop = 13
          ExplicitHeight = 199
        end
        object MemoQueryStrings: TMemo
          Left = 1
          Top = 14
          Width = 575
          Height = 82
          Align = alClient
          TabOrder = 1
          ExplicitLeft = 0
          ExplicitTop = 110
          ExplicitWidth = 320
          ExplicitHeight = 144
        end
      end
      object MemoJSONResult: TMemo
        Left = 0
        Top = 110
        Width = 288
        Height = 144
        Align = alClient
        TabOrder = 2
        ExplicitWidth = 281
      end
    end
    object tabAdd: TTabSheet
      Caption = 'Add'
      ImageIndex = 1
      ExplicitHeight = 77
      object Button2: TButton
        Left = 198
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 0
        OnClick = Button2Click
      end
      object edtPhone: TLabeledEdit
        Left = 103
        Top = 22
        Width = 89
        Height = 21
        EditLabel.Width = 30
        EditLabel.Height = 13
        EditLabel.Caption = 'Phone'
        TabOrder = 1
        Text = '12345'
      end
      object edtName: TLabeledEdit
        Left = 8
        Top = 22
        Width = 89
        Height = 21
        EditLabel.Width = 27
        EditLabel.Height = 13
        EditLabel.Caption = 'Name'
        TabOrder = 2
        Text = 'Stephen'
      end
    end
    object tabDelete: TTabSheet
      Caption = 'Delete'
      ImageIndex = 2
      ExplicitHeight = 77
      object btnDelete: TButton
        Left = 200
        Top = 28
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 0
        OnClick = btnDeleteClick
      end
      object edtDeleteObjectID: TLabeledEdit
        Left = 3
        Top = 30
        Width = 191
        Height = 21
        EditLabel.Width = 80
        EditLabel.Height = 13
        EditLabel.Caption = 'Delete Object ID'
        TabOrder = 1
      end
    end
  end
  object BackendQuery1: TBackendQuery
    Provider = DataModule2.KinveyProvider1
    BackendClassName = 'Customer'
    BackendService = 'Storage'
    Left = 360
    Top = 72
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 420
    Top = 21
    object LinkControlToFieldJSONResult: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BackendQuery1
      FieldName = 'JSONResult'
      Control = MemoJSONResult
      Track = False
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BackendQuery1
      FieldName = 'QueryStrings'
      Control = MemoQueryStrings
      Track = False
    end
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BackendQuery1
      FieldName = 'BackendClassName'
      Control = EditBackendClassName
      Track = True
    end
    object LinkFillControlToField1: TLinkFillControlToField
      Category = 'Quick Bindings'
      DataSource = BackendQuery1
      FieldName = 'BackendService'
      Control = ComboBoxBackendService
      Track = True
      AutoFill = True
      FillExpressions = <>
      FillHeaderExpressions = <>
      FillBreakGroups = <>
    end
  end
  object BackendStorage1: TBackendStorage
    Provider = DataModule2.KinveyProvider1
    Left = 456
    Top = 72
  end
end
