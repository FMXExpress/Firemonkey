unit UPayPalDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomPayPal,
  FMX.TMSCloudPayPal, FMX.ListView.Types, FMX.ListView, System.Rtti,
  FMX.Layouts, FMX.Grid, FMX.TMSCloudListView, FMX.Edit, FMX.ListBox
  {$IFDEF VER280}
  , FMX.Controls.Presentation, FMX.EditBox, FMX.SpinBox
  {$ENDIF}
  ;

type
  TForm7 = class(TForm)
    Button1: TButton;
    TMSFMXCloudPayPal1: TTMSFMXCloudPayPal;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    SpinBox1: TSpinBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbName1: TLabel;
    lbDesc1: TLabel;
    lbPrice1: TLabel;
    Label9: TLabel;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    cbCurrency: TComboBox;
    cbShipping: TComboBox;
    cbInsurance: TCheckBox;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lbStatus: TLabel;
    lbID: TLabel;
    lbName: TLabel;
    lbAddress: TLabel;
    lbCountry: TLabel;
    lbTotal: TLabel;
    lbPrice2: TLabel;
    lbDesc2: TLabel;
    lbName2: TLabel;
    SpinBox2: TSpinBox;
    CheckBox2: TCheckBox;
    lbPrice3: TLabel;
    lbDesc3: TLabel;
    lbName3: TLabel;
    SpinBox3: TSpinBox;
    CheckBox3: TCheckBox;
    lbPrice4: TLabel;
    lbDesc4: TLabel;
    lbName4: TLabel;
    SpinBox4: TSpinBox;
    CheckBox4: TCheckBox;
    lbPrice7: TLabel;
    lbDesc7: TLabel;
    lbName7: TLabel;
    SpinBox7: TSpinBox;
    CheckBox7: TCheckBox;
    lbPrice5: TLabel;
    lbDesc5: TLabel;
    lbName5: TLabel;
    SpinBox5: TSpinBox;
    CheckBox5: TCheckBox;
    lbPrice6: TLabel;
    lbDesc6: TLabel;
    lbName6: TLabel;
    SpinBox6: TSpinBox;
    CheckBox6: TCheckBox;
    Label6: TLabel;
    lbSubTotal: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudPayPal1PaymentAccepted(Sender: TObject);
    procedure TMSFMXCloudPayPal1PaymentCancelled(Sender: TObject);
    procedure TMSFMXCloudPayPal1PaymentFailed(Sender: TObject);
    procedure TMSFMXCloudPayPal1AuthFormClose(Sender: TObject);
    procedure cbCurrencyChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Success: boolean;
    Cancel: boolean;
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  PayPalAppkey = 'xxxxxxxxx';
//  PayPalAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm7.Button1Click(Sender: TObject);
var
  pi: TPayPalItem;
  I: Integer;
  ItemSelected: boolean;
begin
  Success := false;
  Cancel := false;
  ItemSelected := false;

  case cbCurrency.ItemIndex of
    0: TMSFMXCloudPayPal1.Transaction.Currency := pcEUR;
    1: TMSFMXCloudPayPal1.Transaction.Currency := pcUSD;
    2: TMSFMXCloudPayPal1.Transaction.Currency := pcNOK;
    3: TMSFMXCloudPayPal1.Transaction.Currency := pcHKD;
  end;

  if cbInsurance.IsChecked then
    TMSFMXCloudPayPal1.Transaction.Insurance := 5
  else
    TMSFMXCloudPayPal1.Transaction.Insurance := 0;

  TMSFMXCloudPayPal1.Transaction.Shipping := 5;

  TMSFMXCloudPayPal1.Transaction.Items.Clear;

  for I := 1 to 7 do
  begin
    if (Form7.FindComponent('CheckBox' + IntToStr(I)) is TCheckBox) then
    begin
      if (Form7.FindComponent('CheckBox' + IntToStr(I)) as TCheckBox).IsChecked then
      begin
        ItemSelected := true;
        pi := TMSFMXCloudPayPal1.Transaction.Items.Add;
        pi.Price := StrToFloat((Form7.FindComponent('lbPrice' + IntToStr(I)) as TLabel).Text);
        pi.Name := (Form7.FindComponent('lbName' + IntToStr(I)) as TLabel).Text;
        pi.Description := (Form7.FindComponent('lbDesc' + IntToStr(I)) as TLabel).Text;
        pi.Quantity := Round((Form7.FindComponent('SpinBox' + IntToStr(I)) as TSpinBox).Value);
      end;
    end;
  end;

  if ItemSelected then
  begin
    Button1.Enabled := false;
    TMSFMXCloudPayPal1.DoPayment
  end
  else
  begin
    ShowMessage('Please select at least 1 product from the list');
  end;
end;

procedure TForm7.cbCurrencyChange(Sender: TObject);
var
  Currency: string;
begin
  case cbCurrency.ItemIndex of
    0: begin Currency := 'EUR'; cbShipping.ItemIndex := 0; end;
    1: begin Currency := 'USD'; cbShipping.ItemIndex := 1; end;
    2: begin Currency := 'NOK'; cbShipping.ItemIndex := 0; end;
    3: begin Currency := 'HKD'; cbShipping.ItemIndex := 2; end;
  end;

  cbInsurance.Text := 'Insurance (5 ' + Currency + ')';
end;

procedure TForm7.CheckBox1Change(Sender: TObject);
var
  I, quantity, price, subtotal: integer;
begin
  subtotal := 0;
  for I := 1 to 7 do
  begin
    if (Form7.FindComponent('CheckBox' + IntToStr(I)) is TCheckBox) then
    begin
      if (Form7.FindComponent('CheckBox' + IntToStr(I)) as TCheckBox).IsChecked then
      begin
        price := StrToInt((Form7.FindComponent('lbPrice' + IntToStr(I)) as TLabel).Text);
        quantity := Round((Form7.FindComponent('SpinBox' + IntToStr(I)) as TSpinBox).Value);
        subtotal := subtotal + (price * quantity);
      end;
    end;
  end;
  lbSubTotal.Text := IntToStr(subtotal);
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  lbStatus.Text := '';
  lbID.Text := '';
  lbName.Text := '';
  lbAddress.Text := '';
  lbCountry.Text := '';
  lbTotal.Text := '';

  TMSFMXCloudPayPal1.App.Key := PayPalAppKey;
  TMSFMXCloudPayPal1.App.Secret := PayPalAppSecret;
end;

procedure TForm7.TMSFMXCloudPayPal1AuthFormClose(Sender: TObject);
begin
  Button1.Enabled := true;
  if (not Success) and (not Cancel) then
    ShowMessage('Payment cancelled by user');
end;

procedure TForm7.TMSFMXCloudPayPal1PaymentAccepted(Sender: TObject);
begin
  Button1.Enabled := true;
  Success := true;
  lbStatus.Text := 'Payment OK (' + TMSFMXCloudPayPal1.Payment.State + ')';
  lbID.Text := TMSFMXCloudPayPal1.Payment.TransactionID;
  lbName.Text := TMSFMXCloudPayPal1.Payment.Payer.FirstName + ' ' + TMSFMXCloudPayPal1.Payment.Payer.LastName;
  lbAddress.Text := TMSFMXCloudPayPal1.Payment.Payer.Street + ', ' + TMSFMXCloudPayPal1.Payment.Payer.City;
  lbCountry.Text := TMSFMXCloudPayPal1.Payment.Payer.CountryCode;
  lbTotal.Text := TMSFMXCloudPayPal1.Payment.Total + ' ' + TMSFMXCloudPayPal1.Payment.Currency;
end;

procedure TForm7.TMSFMXCloudPayPal1PaymentCancelled(Sender: TObject);
begin
  Button1.Enabled := true;
  Cancel := true;
  ShowMessage('Payment cancelled by user');
end;

procedure TForm7.TMSFMXCloudPayPal1PaymentFailed(Sender: TObject);
begin
  Button1.Enabled := true;
  ShowMessage('Payment failed' + #13 + TMSFMXCloudPayPal1.PaymentError.ErrorMessage);
end;

end.
