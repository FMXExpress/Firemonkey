unit uShow;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  duck, FMX.Controls.Presentation;

type
  TfShow = class(TForm)
    btnShow: TButton;
    procedure btnShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FVisible : Boolean;
  public
    { Public declarations }
  end;

var
  fShow: TfShow;

implementation
{$R *.fmx}
{------------------------------------------------------------------------------}
procedure TfShow.btnShowClick(Sender: TObject);
begin
    FVisible := not FVisible;
    if not Assigned(Owner) then
        Exit;
    self.Owner.duck.all.isa(TButton).each(
    procedure(obj: TObject)
    begin
        obj.duck.setTo('visible', FVisible);
    end
    );
end;
{------------------------------------------------------------------------------}
procedure TfShow.FormCreate(Sender: TObject);
begin
    FVisible := True;
end;
{------------------------------------------------------------------------------}
end.
