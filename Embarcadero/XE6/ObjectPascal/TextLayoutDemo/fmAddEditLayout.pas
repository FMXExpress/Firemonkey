
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fmAddEditLayout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Colors, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.Text, FMX.TextLayout;

type
  TAddEditLayoutForm = class(TForm)
    Label10: TLabel;
    mText: TMemo;
    GroupBox5: TGroupBox;
    rbLeft: TRadioButton;
    rbCenter: TRadioButton;
    rbRight: TRadioButton;
    Label6: TLabel;
    ccbColor: TComboColorBox;
    GroupBox6: TGroupBox;
    sbLeft: TSpinBox;
    Label15: TLabel;
    sbRight: TSpinBox;
    Label12: TLabel;
    sbBottom: TSpinBox;
    Label13: TLabel;
    sbTop: TSpinBox;
    Label14: TLabel;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    cbStrikeOut: TCheckBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    nbSize: TNumberBox;
    Label11: TLabel;
    Label1: TLabel;
    cbxFontFamily: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    btnApply: TButton;
    btnCancel: TButton;
    cbWrap: TCheckBox;
    Label2: TLabel;
    sbOpacity: TSpinBox;
    cbxTrimming: TComboBox;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    lblTrimming: TLabel;
    cbRightToLeft: TCheckBox;
    procedure mTextChange(Sender: TObject);
    procedure cbWrapChange(Sender: TObject);
  public
    procedure SetDefaults;
    //Set controls states from layout properties
    procedure Layout2Controls(ALayout: TTextLayout);
    //Set layout properties from controls states
    procedure Controls2Layout(ALayout: TTextLayout);
  end;


implementation

{$R *.fmx}

{ TAddEditLayoutForm }

procedure TAddEditLayoutForm.cbWrapChange(Sender: TObject);
begin
  lblTrimming.Enabled := not cbWrap.IsChecked;
  cbxTrimming.Enabled := not cbWrap.IsChecked;
end;

procedure TAddEditLayoutForm.Controls2Layout(ALayout: TTextLayout);
begin
  if not Assigned(ALayout) then
    Exit;

  ALayout.BeginUpdate;
  //Text
  ALayout.Text := mText.Text;
  ALayout.WordWrap := cbWrap.IsChecked;
  ALayout.Trimming := TTextTrimming(cbxTrimming.ItemIndex);
  ALayout.RightToLeft := cbRightToLeft.IsChecked;
  //Align
  if rbLeft.IsChecked then
    ALayout.HorizontalAlign := TTextAlign.Leading
  else
    if rbCenter.IsChecked then
      ALayout.HorizontalAlign := TTextAlign.Center
    else
      if rbRight.IsChecked then
        ALayout.HorizontalAlign := TTextAlign.Trailing;
  //Font
  ALayout.Font.Family := cbxFontFamily.Selected.Text;
  ALayout.Font.Size := nbSize.Value;
  ALayout.Color := ccbColor.Color;
  ALayout.Opacity := sbOpacity.Value;
  //Font style
  if cbItalic.IsChecked then
    ALayout.Font.Style := ALayout.Font.Style + [TFontStyle.fsItalic]
  else
    ALayout.Font.Style := ALayout.Font.Style - [TFontStyle.fsItalic];
  if cbBold.IsChecked then
    ALayout.Font.Style := ALayout.Font.Style + [TFontStyle.fsBold]
  else
    ALayout.Font.Style := ALayout.Font.Style - [TFontStyle.fsBold];
  if cbUnderline.IsChecked then
    ALayout.Font.Style := ALayout.Font.Style + [TFontStyle.fsUnderline]
  else
    ALayout.Font.Style := ALayout.Font.Style - [TFontStyle.fsUnderline];
  if cbStrikeOut.IsChecked then
    ALayout.Font.Style := ALayout.Font.Style + [TFontStyle.fsStrikeOut]
  else
    ALayout.Font.Style := ALayout.Font.Style - [TFontStyle.fsStrikeOut];
  //Padding
  ALayout.Padding.Rect := RectF(sbLeft.Value, sbTop.Value, sbRight.Value,
    sbBottom.Value);
  ALayout.EndUpdate;
end;

procedure TAddEditLayoutForm.Layout2Controls(ALayout: TTextLayout);
var
  Idx: Integer;
begin
  if not Assigned(ALayout) then
    Exit;

  //Text
  mText.Text := ALayout.Text;
  cbWrap.IsChecked := ALayout.WordWrap;
  cbxTrimming.ItemIndex := Ord(ALayout.Trimming);
  cbRightToLeft.IsChecked := ALayout.RightToLeft;
  //Align
  case ALayout.HorizontalAlign of
    TTextAlign.Center:
      rbCenter.IsChecked := True;
    TTextAlign.Leading:
      rbLeft.IsChecked := True;
    TTextAlign.Trailing:
      rbRight.IsChecked := True;
  end;
  //Font
  Idx := cbxFontFamily.Items.IndexOf(ALayout.Font.Family);
  if Idx < 0 then
    //Setting default font
    Idx := 2;
  cbxFontFamily.ItemIndex := Idx;
  nbSize.Value := ALayout.Font.Size;
  ccbColor.Color := ALayout.Color;
  sbOpacity.Value := ALayout.Opacity;
  //Font style
  cbItalic.IsChecked := TFontStyle.fsItalic in ALayout.Font.Style;
  cbBold.IsChecked := TFontStyle.fsBold in ALayout.Font.Style;
  cbUnderline.IsChecked := TFontStyle.fsUnderline in ALayout.Font.Style;
  cbStrikeOut.IsChecked := TFontStyle.fsStrikeOut in ALayout.Font.Style;
  //Padding
  sbLeft.Value := ALayout.Padding.Left;
  sbTop.Value := ALayout.Padding.Top;
  sbRight.Value := ALayout.Padding.Right;
  sbBottom.Value := ALayout.Padding.Bottom;
end;

procedure TAddEditLayoutForm.mTextChange(Sender: TObject);
begin
  btnApply.Enabled := mText.Text <> EmptyStr;
end;

procedure TAddEditLayoutForm.SetDefaults;
begin
  //Text
  mText.Text := EmptyStr;
  cbWrap.IsChecked := False;
  cbxTrimming.ItemIndex := 0;
  cbRightToLeft.IsChecked := False;
  //Align
  rbLeft.IsChecked := True;
  //Font
  cbxFontFamily.ItemIndex := 2;
  nbSize.Value := 20;
  ccbColor.Color := TAlphaColorRec.Black;
  sbOpacity.Value := 1;
  //Font style
  cbItalic.IsChecked := False;
  cbBold.IsChecked := False;
  cbUnderline.IsChecked := False;
  cbStrikeOut.IsChecked := False;
  //Padding
  sbLeft.Value := 0;
  sbTop.Value := 0;
  sbRight.Value := 0;
  sbBottom.Value := 0;
end;

end.
