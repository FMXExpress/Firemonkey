//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  FMX.Advertising, FMX.InAppPurchase, System.Generics.Collections, FMX.Layouts, FMX.MultiView, FMX.ListBox,
  FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    bFiveQuestion: TButton;
    bTenQuestion: TButton;
    bTwentyQuestion: TButton;
    Label1: TLabel;
    ToolBar1: TToolBar;
    Button4: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    sEast: TSwitch;
    ListBoxItem2: TListBoxItem;
    sCentral: TSwitch;
    ListBoxItem3: TListBoxItem;
    sWest: TSwitch;
    ListBoxItem4: TListBoxItem;
    sEurope: TSwitch;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    DisableAdsListBoxItem: TListBoxItem;
    EuropeListBoxItem: TListBoxItem;
    ConsumeListBoxItem: TListBoxItem;
    RestoreAdsListBoxItem: TListBoxItem;
    ToolBar2: TToolBar;
    Label2: TLabel;
    Button1: TButton;
    procedure bFiveQuestionClick(Sender: TObject);
    procedure bTenQuestionClick(Sender: TObject);
    procedure bTwentyQuestionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure InAppPurchaseSetupComplete(Sender: TObject);
    procedure InAppPurchaseProductsRequestResponse(Sender: TObject;
      const Products: TIAPProductList; const InvalidProductIDs: TStrings);
    procedure InAppPurchaseError(Sender: TObject;
      FailureKind: TFailureKind; const ErrorMessage: string);
    procedure InAppPurchasePurchaseCompleted(Sender: TObject;
      const ProductID: string; NewTransaction: Boolean);
    procedure MultiView1Hidden(Sender: TObject);
    procedure OptionFormButton1Click(Sender: TObject);
    procedure sEastSwitch(Sender: TObject);
    procedure sCentralSwitch(Sender: TObject);
    procedure sWestSwitch(Sender: TObject);
    procedure sEuropeSwitch(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  private
    FWest: Boolean;
    FEast: Boolean;
    FCentral: Boolean;
    FEurope: Boolean;
    FBannerAd: TBannerAd;
    FInAppPurchase: TInAppPurchase;
    FNoAdsProductIsValid: Boolean;
    FEuropeProductIsValid: Boolean;
    procedure BannerAdDidLoad(Sender: TObject);
    procedure BannerAdWillLoad(Sender: TObject);
    procedure BannerAdDidFail(Sender: TObject; const Error: string);
    procedure ConsumeComplete(Sender: TObject; const ProductID: string);
    procedure ConsumeFailed(Sender: TObject; const ProductID, ErrorMessage: string);
    procedure CheckState;
  public
    property West: Boolean read FWest write FWest;
    property East: Boolean read FEast write FEast;
    property Central: Boolean read FCentral write FCentral;
    property Europe: Boolean read FEurope write FEurope;
    property NoAdsProductIsValid: Boolean read FNoAdsProductIsValid;
    property EuropeProductIsValid: Boolean read FEuropeProductIsValid;
    property InAppPurchase: TInAppPurchase read FInAppPurchase;
    procedure OptionsDone;
    procedure TakeAdvertFromMainForm(Form: TCustomForm);
    procedure PlaceAdvertOnMainForm;
    procedure DisableAdverts;
    procedure PurchaseEurope;
    procedure DisablePurchaseUI(BecauseOfPurchase: Boolean);
    procedure DisableEuropePurchaseUI(BecauseOfPurchase: Boolean);
    procedure EnablePurchaseUI;
    procedure EnableEuropePurchaseUI;
    procedure RestorePurchase;
    procedure ConsumeProducts;
  end;

var
  MainForm: TMainForm;

implementation

uses Quiz, Score, Answered;

{$R *.fmx}

const
  EuropeID = 'com.embarcadero.capitals.europe';
  NoAdsID = 'com.embarcadero.capitals.noads';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Log.d('Main form alive');
  FInAppPurchase := TInAppPurchase.Create(Self);
  // Set the license Key for Android only
  // You can get this license from your Google Dev. Console.
  FInAppPurchase.ApplicationLicenseKey := '';
  FInAppPurchase.ProductIDs.Add(NoAdsID);
  FInAppPurchase.ProductIDs.Add(EuropeID);
  FInAppPurchase.OnSetupComplete := InAppPurchaseSetupComplete;
  FInAppPurchase.OnProductsRequestResponse := InAppPurchaseProductsRequestResponse;
  FInAppPurchase.OnError := InAppPurchaseError;
  FInAppPurchase.OnPurchaseCompleted := InAppPurchasePurchaseCompleted;
  FInAppPurchase.OnConsumeCompleted := ConsumeComplete;
  FInAppPurchase.OnConsumeFailed := ConsumeFailed;
  Log.d('Setting up IAP');
  FInAppPurchase.SetupInAppPurchase;
  East := True;
  West := True;
  Central := True;
  CreateQuiz;
  CreateScore(QuizForm);
  CreateAnswer(QuizForm);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Log.d('Main form active');
  PlaceAdvertOnMainForm
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  Log.d('Main form inactive now');
end;

procedure TMainForm.InAppPurchaseError(Sender: TObject;
  FailureKind: TFailureKind; const ErrorMessage: string);
begin
  Log.d('Purchasing error: ' + ErrorMessage);
end;

procedure TMainForm.InAppPurchaseProductsRequestResponse(Sender: TObject;
  const Products: TIAPProductList; const InvalidProductIDs: TStrings);
var
  Product: TProduct;
begin
  Log.d('TMainForm.InAppPurchaseProductsRequestResponse');
  for Product in Products do
  begin
    if Product.ProductID = NoAdsId then
      FNoAdsProductIsValid := True;
    if Product.ProductID = EuropeID then
      FEuropeProductIsValid := True;
  end;
  if not FNoAdsProductIsValid then
    DisablePurchaseUI(False);
  if not FEuropeProductIsValid then
    DisableEuropePurchaseUI(False);
end;

procedure TMainForm.InAppPurchasePurchaseCompleted(Sender: TObject;
  const ProductID: string; NewTransaction: Boolean);
begin
  Log.d('TMainForm.InAppPurchasePurchaseCompleted');
  if ProductID = NoAdsID then
  begin
    FBannerAd.Hide;
    FBannerAd.DisposeOf;
    FBannerAd := nil;
    DisablePurchaseUI(True);
  end;
  if ProductID = EuropeID then
  begin
    DisableEuropePurchaseUI(True);
  end;
end;

procedure TMainForm.InAppPurchaseSetupComplete(Sender: TObject);
begin
  Log.d('TMainForm.InAppPurchaseSetupComplete');
  FInAppPurchase.QueryProducts;
  if FInAppPurchase.IsProductPurchased(NoAdsID) then
    DisablePurchaseUI(True)
  else
  begin
    Log.d('Creating banner ad');
    FBannerAd := TBannerAd.Create(Self);
    FBannerAd.Parent := Self;
    // For Android set this ID that represents the AdMob Unit.
    FBannerAd.AdUnitID := '';

    Log.d('Banner ad created');
    FBannerAd.Align := TAlignLayout.Bottom;
    FBannerAd.Hide;
    FBannerAd.OnDidLoad := BannerAdDidLoad;
    FBannerAd.OnWillLoad := BannerAdWillLoad;
    FBannerAd.OnDidFail := BannerAdDidFail;
    FBannerAd.LoadAd;
    Log.d('Banner ad properties set');
    EnablePurchaseUI;
  end;

  if FInAppPurchase.IsProductPurchased(EuropeID) then
    DisableEuropePurchaseUI(True)
  else
  begin
    EnableEuropePurchaseUI;
  end;
end;

procedure TMainForm.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if Item = DisableAdsListBoxItem then
    DisableAdverts;
  if Item = EuropeListBoxItem then
    PurchaseEurope;
  if Item = ConsumeListBoxItem then
    ConsumeProducts;
  if Item = RestoreAdsListBoxItem then
    RestorePurchase;
end;

procedure TMainForm.MultiView1Hidden(Sender: TObject);
begin
  OptionsDone;
end;

procedure TMainForm.BannerAdDidFail(Sender: TObject; const Error: string);
begin
  Log.d('TMainForm.BannerAdDidFail: %s', [Error]);
  (Sender as TBannerAd).Hide;
end;

procedure TMainForm.BannerAdDidLoad(Sender: TObject);
begin
  Log.d('TMainForm.BannerAdDidLoad');
  (Sender as TBannerAd).Show;

end;

procedure TMainForm.BannerAdWillLoad(Sender: TObject);
begin
  Log.d('TMainForm.BannerAdWillLoad');
end;

procedure TMainForm.bFiveQuestionClick(Sender: TObject);
begin
  StartQuiz(5);
end;

procedure TMainForm.bTenQuestionClick(Sender: TObject);
begin
  StartQuiz(10);
end;

procedure TMainForm.bTwentyQuestionClick(Sender: TObject);
begin
  StartQuiz(20);
end;

procedure TMainForm.CheckState;
begin
  sCentral.HitTest := sEast.IsChecked or sWest.IsChecked or (sEurope.IsChecked and ListBoxItem4.Visible);
  sEast.HitTest := sCentral.IsChecked or sWest.IsChecked or (sEurope.IsChecked and ListBoxItem4.Visible);
  sWest.HitTest := sCentral.IsChecked or sEast.IsChecked or (sEurope.IsChecked and ListBoxItem4.Visible);
  sEurope.HitTest := sCentral.IsChecked or sEast.IsChecked or sWest.IsChecked;
end;

procedure TMainForm.ConsumeComplete(Sender: TObject; const ProductID: string);
begin
  Log.d('Consume Complete: ' + ProductID);
end;

procedure TMainForm.ConsumeFailed(Sender: TObject; const ProductID, ErrorMessage: string);
begin
  Log.d('Consume Failed: ' + ProductID);
end;

procedure TMainForm.ConsumeProducts;
var
  products: TStringList;
begin
  products := TStringList.Create;
  if FInAppPurchase.IsProductPurchased(EuropeID) then
    products.Add(EuropeID);
  if FInAppPurchase.IsProductPurchased(NoAdsID) then
    products.Add(NoAdsID);
  FInAppPurchase.ConsumeProducts(products);
  Log.d('Consuming Items');
end;

procedure TMainForm.DisableAdverts;
begin
  FInAppPurchase.PurchaseProduct(NoAdsID);
end;

procedure TMainForm.DisableEuropePurchaseUI(BecauseOfPurchase: Boolean);
begin
  ConsumeListBoxItem.Enabled := True;
  EuropeListBoxItem.Enabled := False;
  EuropeListBoxItem.OnClick := nil;
  if BecauseOfPurchase then
  begin
    EuropeListBoxItem.Text := 'Europe pack is now owned';
    ListBoxItem4.Visible  := True;
    Europe := True;
  end;
end;

procedure TMainForm.DisablePurchaseUI(BecauseOfPurchase: Boolean);
begin
  ConsumeListBoxItem.Enabled := True;
  DisableAdsListBoxItem.Enabled := False;
  DisableAdsListBoxItem.OnClick := nil;
  if BecauseOfPurchase then
    DisableAdsListBoxItem.Text := 'In-app adverts are disabled';
end;

procedure TMainForm.EnableEuropePurchaseUI;
begin
  EuropeListBoxItem.Enabled := True;
end;

procedure TMainForm.EnablePurchaseUI;
begin
  DisableAdsListBoxItem.Enabled := True;
  RestoreAdsListBoxItem.Enabled := True;
end;

procedure TMainForm.OptionFormButton1Click(Sender: TObject);
begin
  MultiView1.HideMaster;
end;

procedure TMainForm.OptionsDone;
begin
  if West and (not East) and (not Central) then
    bTwentyQuestion.Visible := False
  else if (not West) and (not East) and Central then
    bTwentyQuestion.Visible := False
  else
    bTwentyQuestion.Visible := True;
end;

procedure TMainForm.PlaceAdvertOnMainForm;
begin
  Log.d('Main form setting ad parent back to itself');
  if FBannerAd <> nil then
    FBannerAd.Parent := Self;
end;

procedure TMainForm.PurchaseEurope;
begin
{$IFDEF WIN32}  // For Testing
  EnableEuropePurchaseUI;
  DisableEuropePurchaseUI(True);
{$ELSE}
  FInAppPurchase.PurchaseProduct(EuropeID);
  EnablePurchaseUI;
{$ENDIF}
end;

procedure TMainForm.RestorePurchase;
begin
  Log.d('Restoring purchases');
  FInAppPurchase.RestorePurchasedProducts;
end;

procedure TMainForm.sCentralSwitch(Sender: TObject);
begin
  Central := sCentral.IsChecked;
  CheckState;
end;

procedure TMainForm.sEastSwitch(Sender: TObject);
begin
  East := sEast.IsChecked;
  CheckState;
end;

procedure TMainForm.sEuropeSwitch(Sender: TObject);
begin
  Europe := sEurope.IsChecked;
  CheckState;
end;

procedure TMainForm.sWestSwitch(Sender: TObject);
begin
  West := sWest.IsChecked;
  CheckState;
end;

procedure TMainForm.TakeAdvertFromMainForm(Form: TCustomForm);
begin
  if FBannerAd <> nil then
    FBannerAd.Parent := Form;
end;

end.
