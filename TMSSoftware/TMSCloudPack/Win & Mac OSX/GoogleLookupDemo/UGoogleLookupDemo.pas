unit UGoogleLookupDemo;

interface

uses
  FMX.TMSCloudGoogleLookup, FMX.TMSCloudBase, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Edit, System.Classes, FMX.Types, FMX.Controls, FMX.Forms;

type
  TForm1 = class(TForm)
    TMSFMXCloudGoogleLookupProvider1: TTMSFMXCloudGoogleLookupProvider;
    TMSFMXCloudGoogleLocationLookupProvider1: TTMSFMXCloudGoogleLocationLookupProvider;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    ListBox1: TListBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  GAppkey = 'xxxxxxxxx';
//  GAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
  TMSFMXCloudGoogleLookupProvider1.Lookup(Edit1.Text, ListBox1.Items);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXCloudGoogleLocationLookupProvider1.App.Key := GAppKey;
end;

end.
