unit UGoogleLookUpDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Edit, FMX.TMSCloudGoogleLookup, FMX.TMSCloudBase;

type
  TForm82 = class(TForm)
    TMSFMXCloudGoogleLocationLookupProvider1: TTMSFMXCloudGoogleLocationLookupProvider;
    TMSFMXCloudGoogleLookupProvider1: TTMSFMXCloudGoogleLookupProvider;
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form82: TForm82;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  GAppkey = 'xxxxxxxxx';
//  GAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
  if RadioButton1.IsChecked then
    TMSFMXCloudGoogleLookupProvider1.Lookup(Edit1.Text, ListBox1.Items)
  else
    TMSFMXCloudGoogleLocationLookupProvider1.Lookup(Edit1.Text, ListBox1.Items);
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  TMSFMXCloudGoogleLookupProvider1.App.Key := GAppKey;
  TMSFMXCloudGoogleLocationLookupProvider1.App.Key := GAppkey;
end;

end.
