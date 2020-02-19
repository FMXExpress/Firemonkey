//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView, FMX.ListView.Appearances;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ListView1PullRefresh(Sender: TObject);
  private
    { Private declarations }
    function GetRandomText: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
  RandomTextUtils;

const
  TotalListItems = 20;

{ TForm1 }

function TForm1.GetRandomText: string;
begin
  Result := CommonNames[Random(20)] + ' ' + CommonSurNames[Random(20)] + ' (' + SampleTopics[Random(10)] + ')';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  Item: TListViewItem;
begin
  for I := 0 to TotalListItems - 1 do
  begin
    Item := ListView1.Items.Add;
    Item.Text :=  GetRandomText;
    Item.Height := 56;
  end;
end;

procedure TForm1.ListView1PullRefresh(Sender: TObject);
var
  Item: TListViewItem;
begin
  Item := ListView1.Items.Insert(0);
  Item.Text :=  GetRandomText;
  Item.Height := 56;

  if ListView1.Items.Count > TotalListItems then
    ListView1.Items.Delete(ListView1.Items.Count - 1);
end;

end.
