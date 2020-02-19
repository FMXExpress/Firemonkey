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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.Edit,
  FMX.StdCtrls;

type
  TForm3 = class(TForm)
    TextEdit: TEdit;
    Rectangle1: TRectangle;
    TextControl: TText;
    Label1: TLabel;
    TextRadioButton: TRadioButton;
    ImageRadioButton: TRadioButton;
    Panel1: TPanel;
    procedure UpdateTextControl(Sender: TObject);
    procedure TextControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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
  FMX.Platform;


procedure TForm3.TextControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Svc: IFMXDragDropService;
  DragData: TDragObject;
  DragImage: TBitmap;
begin
  if not TextEdit.Text.IsEmpty and TPlatformServices.Current.SupportsPlatformService(IFMXDragDropService, Svc) then
  begin
    DragData.Source := Sender;
    DragImage := Rectangle1.MakeScreenshot;
    try
      if TextRadioButton.IsChecked then
        DragData.Data := TextEdit.Text
      else
        DragData.Data := DragImage;
      Svc.BeginDragDrop(Self, DragData, DragImage);
    finally
      DragImage.Free;
    end;
  end;
end;

procedure TForm3.UpdateTextControl(Sender: TObject);
begin
  TextControl.Text := TextEdit.Text;
end;

end.
