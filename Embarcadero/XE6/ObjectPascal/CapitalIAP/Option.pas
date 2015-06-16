unit Option;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Layouts;

type
  TOptionForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Button1: TButton;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    sEast: TSwitch;
    sCentral: TSwitch;
    sWest: TSwitch;
    ListBoxItem4: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    DisableAdsListBoxItem: TListBoxItem;
    RestoreAdsListBoxItem: TListBoxItem;
    sEurope: TSwitch;
    EuropeListBoxItem: TListBoxItem;
    ConsumeListBoxItem: TListBoxItem;
    procedure Button1Click(Sender: TObject);
    procedure sWestSwitch(Sender: TObject);
    procedure sCentralSwitch(Sender: TObject);
    procedure sEastSwitch(Sender: TObject);
    procedure DisableAdsListBoxItemClick(Sender: TObject);
    procedure RestoreAdsListBoxItemClick(Sender: TObject);
    procedure EuropeListBoxItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ConsumeListBoxItemClick(Sender: TObject);
  private
    procedure CheckState;
  public
    procedure DisablePurchaseItem;
  end;

var
  OptionForm: TOptionForm = nil;

procedure CreateOptions;
procedure ShowOptions;

implementation

uses Main;

{$R *.fmx}

procedure CreateOptions;
begin
  if not Assigned(OptionForm) then
    OptionForm := TOptionForm.Create(MainForm);
end;

procedure ShowOptions;
begin
  if not Assigned(OptionForm) then
    OptionForm := TOptionForm.Create(MainForm);
  OptionForm.Show;
end;

procedure TOptionForm.Button1Click(Sender: TObject);
begin
  Close;
  MainForm.OptionsDone;
end;

procedure TOptionForm.CheckState;
begin
  sCentral.HitTest := sEast.IsChecked or sWest.IsChecked;
  sEast.HitTest := sCentral.IsChecked or sWest.IsChecked;
  sWest.HitTest := sCentral.IsChecked or sEast.IsChecked;
end;

procedure TOptionForm.FormShow(Sender: TObject);
begin
  sCentral.IsChecked := MainForm.Central;
  sEast.IsChecked := MainForm.East;
  sWest.IsChecked := MainForm.West;
  CheckState;
end;

procedure TOptionForm.ConsumeListBoxItemClick(Sender: TObject);
begin
  MainForm.ConsumeProducts;
end;

procedure TOptionForm.EuropeListBoxItemClick(Sender: TObject);
begin
  MainForm.PurchaseEurope; // that would be awesome :))
end;

procedure TOptionForm.RestoreAdsListBoxItemClick(Sender: TObject);
begin
  MainForm.RestorePurchase;
end;

procedure TOptionForm.DisableAdsListBoxItemClick(Sender: TObject);
begin
  MainForm.DisableAdverts;
end;

procedure TOptionForm.DisablePurchaseItem;
begin
  Log.d('Disabling the no-ads purchase UI');
  DisableAdsListBoxItem.Enabled := False;
end;

procedure TOptionForm.sCentralSwitch(Sender: TObject);
begin
  MainForm.Central := sCentral.IsChecked;
  CheckState;
end;

procedure TOptionForm.sEastSwitch(Sender: TObject);
begin
  MainForm.East := sEast.IsChecked;
  CheckState;
end;

procedure TOptionForm.sWestSwitch(Sender: TObject);
begin
  MainForm.West := sWest.IsChecked;
  CheckState;
end;

end.
