object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 427
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Touch.GestureManager = GestureManager1
  PixelsPerInch = 96
  TextHeight = 13
  object GestureListView1: TGestureListView
    Left = 360
    Top = 8
    Width = 250
    Height = 150
    Columns = <
      item
        AutoSize = True
        Caption = 'Nom'
      end>
    GestureManager = GestureManager1
    ImageSize = 24
    TabOrder = 0
  end
  object GesturePreview1: TGesturePreview
    Left = 56
    Top = 192
    Color = clAqua
    GestureProvider = GestureListView1
    ParentColor = False
  end
  object GestureRecorder1: TGestureRecorder
    Left = 392
    Top = 200
    Caption = ''
    Color = clGradientActiveCaption
    GestureManager = GestureManager1
    ParentColor = False
  end
  object GestureManager1: TGestureManager
    Left = 72
    Top = 48
  end
  object ActionList1: TActionList
    Left = 208
    Top = 48
  end
end
