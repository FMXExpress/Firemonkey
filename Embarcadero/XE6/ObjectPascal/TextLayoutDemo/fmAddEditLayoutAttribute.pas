
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fmAddEditLayoutAttribute;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Colors, FMX.Edit, FMX.StdCtrls,
  FMX.ListBox, FMX.Text, FMX.TextLayout, FMX.Layouts, FMX.Memo;

type
  TAddEditLayoutAttributeForm = class(TForm)
    Label1: TLabel;
    cbxFontFamily: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    nbSize: TNumberBox;
    Label11: TLabel;
    ccbColor: TComboColorBox;
    Label6: TLabel;
    GroupBox3: TGroupBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeOut: TCheckBox;
    cbBold: TCheckBox;
    btnApply: TButton;
    btnCancel: TButton;
    mText: TMemo;
    Label2: TLabel;
    procedure btnApplyClick(Sender: TObject);
  public
    procedure SetDefaults(const AText: string);
    //Set controls states from layout properties
    procedure Attribute2Controls(const AText: string;
      AAttributedRange: TTextAttributedRange);
    //Set layout properties from controls states
    procedure Controls2Attribute(AAttributedRange: TTextAttributedRange);
  end;


implementation

{$R *.fmx}

{ TAddEditLayoutAttributeForm }

procedure TAddEditLayoutAttributeForm.Attribute2Controls(const AText: string;
    AAttributedRange: TTextAttributedRange);
var
  Idx: Integer;
begin
  mText.Text := AText;
  mText.SelStart := AAttributedRange.Range.Pos;
  mText.SelLength := AAttributedRange.Range.Length;
  //Font
  Idx := cbxFontFamily.Items.IndexOf(AAttributedRange.Attribute.Font.Family);
  if Idx < 0 then
    //Setting default font
    Idx := 2;
  cbxFontFamily.ItemIndex := Idx;
  nbSize.Value := AAttributedRange.Attribute.Font.Size;
  ccbColor.Color := AAttributedRange.Attribute.Color;
  //Font style
  cbItalic.IsChecked := TFontStyle.fsItalic in AAttributedRange.Attribute.Font.Style;
  cbBold.IsChecked := TFontStyle.fsBold in AAttributedRange.Attribute.Font.Style;
  cbUnderline.IsChecked := TFontStyle.fsUnderline in AAttributedRange.Attribute.Font.Style;
  cbStrikeOut.IsChecked := TFontStyle.fsStrikeOut in AAttributedRange.Attribute.Font.Style;
end;

procedure TAddEditLayoutAttributeForm.btnApplyClick(Sender: TObject);
begin
  if mText.SelLength = 0 then
    ModalResult := mrNone;
end;

procedure TAddEditLayoutAttributeForm.Controls2Attribute(AAttributedRange: TTextAttributedRange);
begin
  AAttributedRange.Range.Pos := mText.SelStart;
  AAttributedRange.Range.Length := mText.SelLength;
  //Font
  AAttributedRange.Attribute.Font.Family := cbxFontFamily.Selected.Text;
  AAttributedRange.Attribute.Font.Size := nbSize.Value;
  AAttributedRange.Attribute.Color := ccbColor.Color;
  //Font style
  if cbItalic.IsChecked then
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style + [TFontStyle.fsItalic]
  else
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style - [TFontStyle.fsItalic];
  if cbBold.IsChecked then
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style + [TFontStyle.fsBold]
  else
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style - [TFontStyle.fsBold];
  if cbUnderline.IsChecked then
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style + [TFontStyle.fsUnderline]
  else
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style - [TFontStyle.fsUnderline];
  if cbStrikeOut.IsChecked then
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style + [TFontStyle.fsStrikeOut]
  else
    AAttributedRange.Attribute.Font.Style :=
      AAttributedRange.Attribute.Font.Style - [TFontStyle.fsStrikeOut];
end;

procedure TAddEditLayoutAttributeForm.SetDefaults(const AText: string);
begin
  mText.Text := AText;
  cbxFontFamily.ItemIndex := 2;
  nbSize.Value := 20;
  ccbColor.Color := TAlphaColorRec.Black;
  cbItalic.IsChecked := False;
  cbBold.IsChecked := False;
  cbUnderline.IsChecked := False;
  cbStrikeOut.IsChecked := False;
end;

end.
