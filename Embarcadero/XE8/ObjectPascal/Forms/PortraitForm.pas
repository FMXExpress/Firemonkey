//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit PortraitForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.MobilePreview;

type
  TPForm = class(TForm)
    ToolBar1: TToolBar;
    Label2: TLabel;
    Image1: TImage;
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
  public
    { Public declarations }
  end;

var
  PForm: TPForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
LandscapeForm;

procedure TPForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
{$ifdef ANDROID}
  if Key = vkHardwareBack then
    Key := 0; // avoid the default back action.
{$endif}
end;

procedure TPForm.FormResize(Sender: TObject);
begin
  if (Height < Width) and (Visible) and Assigned(LSForm) then
    LSForm.Show;
end;

end.
