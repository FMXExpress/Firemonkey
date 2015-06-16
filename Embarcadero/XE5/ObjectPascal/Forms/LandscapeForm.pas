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

procedure TLSForm.FormCreate(Sender: TObject);
begin
  FCreated := True;
end;

procedure TLSForm.FormResize(Sender: TObject);
begin
  if (Height > Width) and (Visible) and Assigned(PForm) then
    PForm.Show;
end;

end.
