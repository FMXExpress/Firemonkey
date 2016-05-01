unit Option;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Layouts;

type
  TOptionFrame = class(TFrame)
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
    procedure sWestSwitch(Sender: TObject);
    procedure sCentralSwitch(Sender: TObject);
    procedure sEastSwitch(Sender: TObject);
    procedure DisableAdsListBoxItemClick(Sender: TObject);
    procedure RestoreAdsListBoxItemClick(Sender: TObject);
    procedure EuropeListBoxItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ConsumeListBoxItemClick(Sender: TObject);
    procedure sEuropeSwitch(Sender: TObject);
  private
    procedure CheckState;
  public
    procedure DisablePurchaseItem;
  end;

implementation

uses Main;

{$R *.fmx}

procedure TOptionFrame.CheckState;
begin
  sCentral.HitTest := sEast.IsChecked or sWest.IsChecked;
  sEast.HitTest := sCentral.IsChecked or sWest.IsChecked;
  sWest.HitTest := sCentral.IsChecked or sEast.IsChecked;
end;

procedure TOptionFrame.FormShow(Sender: TObject);
begin
  sCentral.IsChecked := MainForm.Central;
  sEast.IsChecked := MainForm.East;
  sWest.IsChecked := MainForm.West;
  CheckState;
end;

procedure TOptionFrame.ConsumeListBoxItemClick(Sender: TObject);
begin
  MainForm.ConsumeProducts;
end;

procedure TOptionFrame.EuropeListBoxItemClick(Sender: TObject);
begin
  MainForm.PurchaseEurope; // that would be awesome :))
end;

procedure TOptionFrame.RestoreAdsListBoxItemClick(Sender: TObject);
begin
  MainForm.RestorePurchase;
end;

procedure TOptionFrame.DisableAdsListBoxItemClick(Sender: TObject);
begin
  MainForm.DisableAdverts;
end;

procedure TOptionFrame.DisablePurchaseItem;
begin
  Log.d('Disabling the no-ads purchase UI');
  DisableAdsListBoxItem.Enabled := False;
end;

procedure TOptionFrame.sCentralSwitch(Sender: TObject);
begin
  MainForm.Central := sCentral.IsChecked;
  CheckState;
end;

procedure TOptionFrame.sEastSwitch(Sender: TObject);
begin
  MainForm.East := sEast.IsChecked;
  CheckState;
end;

procedure TOptionFrame.sEuropeSwitch(Sender: TObject);
begin
  MainForm.Europe := sEurope.IsChecked;
  CheckState;
end;

procedure TOptionFrame.sWestSwitch(Sender: TObject);
begin
  MainForm.West := sWest.IsChecked;
  CheckState;
end;

end.
