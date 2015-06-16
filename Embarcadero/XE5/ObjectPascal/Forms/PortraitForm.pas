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
  private
  public
    { Public declarations }
  end;

var
  PForm: TPForm;

implementation

{$R *.fmx}

uses
LandscapeForm;

procedure TPForm.FormResize(Sender: TObject);
begin
  if (Height < Width) and (Visible) and Assigned(LSForm) then
    LSForm.Show;
end;

end.
