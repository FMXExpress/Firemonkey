//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit LandscapeForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.MobilePreview;

type
  TLSForm = class(TForm)
    ToolBar1: TToolBar;
    Label2: TLabel;
    Image1: TImage;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FCreated: Boolean;
  public
    { Public declarations }
  end;

var
  LSForm: TLSForm;

implementation

uses
 PortraitForm;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TLSForm.FormCreate(Sender: TObject);
begin
  FCreated := True;
end;

procedure TLSForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
{$ifdef ANDROID}
  if Key = vkHardwareBack then
    Key := 0; // avoid the default back action.
{$endif}
end;

procedure TLSForm.FormResize(Sender: TObject);
begin
  if (Height > Width) and (Visible) and Assigned(PForm) then
    PForm.Show;
end;

end.
