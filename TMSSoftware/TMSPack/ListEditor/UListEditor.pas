unit UListEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TMSListEditor, FMX.ListBox;

type
  TForm1175 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    TMSFMXListEditor1: TTMSFMXListEditor;
    GroupBox2: TGroupBox;
    TMSFMXListEditor2: TTMSFMXListEditor;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXListEditor2ItemClick(Sender: TObject; AItemIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1175: TForm1175;

implementation

{$R *.fmx}

procedure TForm1175.Button1Click(Sender: TObject);
var
  i: integer;
begin
  ListBox1.Items.Clear;
  for i := 0 to TMSFMXListEditor1.Items.Count - 1 do
    ListBox1.Items.Add(TMSFMXListEditor1.Items[i].Text);
end;

procedure TForm1175.FormCreate(Sender: TObject);
begin
  TMSFMXListEditor1.ItemAppearance.RoundingNormal := 0;
  TMSFMXListEditor1.ItemAppearance.RoundingSelected := 0;
  TMSFMXListEditor2.SelectedItemIndex := 0;
  Label2.Text := TMSFMXListEditor2.Items[0].Value;
end;

procedure TForm1175.TMSFMXListEditor2ItemClick(Sender: TObject;
  AItemIndex: Integer);
begin
  Label2.Text := TMSFMXListEditor2.Items[AItemIndex].Value;
end;

end.
