//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit fmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Objects, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Edit, FMX.Colors;

type
  TForm3 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Panel1: TPanel;
    TextBorder: TRectangle;
    TextControl: TText;
    Label1: TLabel;
    Edit1: TEdit;
    TextRadioButton: TRadioButton;
    ImageRadioButton: TRadioButton;
    CopyButton: TButton;
    Panel2: TPanel;
    PasteImage: TImage;
    Button1: TButton;
    PasteLabel: TLabel;
    procedure Edit1ChangeTracking(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses
  System.Rtti, FMX.Platform, FMX.Surfaces;


procedure TForm3.Button1Click(Sender: TObject);
var
  Svc: IFMXClipboardService;
  Value: TValue;
  Bitmap: TBitmap;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Value := Svc.GetClipboard;
    if not Value.IsEmpty then
    begin
      if Value.IsType<string> then
      begin
        PasteLabel.Text := Value.ToString;
        PasteImage.Bitmap := nil;
      end
      else if Value.IsType<TBitmapSurface> then
      try
        PasteLabel.Text := string.Empty;
        Bitmap := TBitmap.Create;
        try
          Bitmap.Assign(Value.AsType<TBitmapSurface>);
          PasteImage.Bitmap := Bitmap;
        finally
          Bitmap.Free;
        end;
      finally
        Value.AsType<TBitmapSurface>.Free;
      end;
    end;
  end;
end;

procedure TForm3.CopyButtonClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
  Image: TBitmap;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    if TextRadioButton.IsChecked then
      Svc.SetClipboard(Edit1.Text)
    else
    begin
      Image := TextBorder.MakeScreenshot;
      try
        Svc.SetClipboard(Image);
      finally
        Image.Free;
      end;
    end;
end;

procedure TForm3.Edit1ChangeTracking(Sender: TObject);
begin
  CopyButton.Enabled := not Edit1.Text.IsEmpty;
  TextControl.Text := Edit1.Text;
end;

end.
