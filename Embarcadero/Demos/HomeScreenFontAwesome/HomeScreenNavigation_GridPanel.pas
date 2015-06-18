unit HomeScreenNavigation_GridPanel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FontAwesomeCodes;

type
  TForm20 = class(TForm)
    ToolBar1: TToolBar;
    GridPanelLayout1: TGridPanelLayout;
    Label7: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SetInCode: TLabel;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form20: TForm20;

implementation

{$R *.fmx}

procedure TForm20.FormActivate(Sender: TObject);
begin
  SetInCode.Text := fa_ambulance;
end;

end.
