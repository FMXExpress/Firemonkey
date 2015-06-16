unit UCategories;

interface

uses
  FMX.Forms, FMX.Controls, FMX.StdCtrls, System.Classes, FMX.Types, FMX.Layouts,
  FMX.TreeView;


type
  TForm2 = class(TForm)
    TreeView1: TTreeView;
    btAddCat: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.FMX}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Self.Close;
end;

end.
