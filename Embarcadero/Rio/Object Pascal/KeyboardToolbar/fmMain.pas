//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit fmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.ListBox, FMX.Layouts, FMX.Menus, FMX.VirtualKeyboard,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    btnAdd: TButton;
    btnDelete: TButton;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    swToolbar: TSwitch;
    swDoneButton: TSwitch;
    lbButtons: TListBox;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxHeader2: TListBoxHeader;
    Label2: TLabel;
    ListBoxItem3: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure swToolbarSwitch(Sender: TObject);
    procedure swDoneButtonSwitch(Sender: TObject);
    procedure lbButtonsChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FService: IFMXVirtualKeyboardToolbarService;
    procedure CustomButtonExecute(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.DialogService;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  if Assigned(FService) then
  begin
    TDialogService.InputQuery('Add new toolbutton', ['Enter button caption'], [''],
      procedure(const AResult: TModalResult; const AValues: array of string)
      var
        LCaption: string;
      begin
        LCaption := AValues[0];
        if not LCaption.IsEmpty then
        begin
          lbButtons.Items.Add(LCaption);
          FService.AddButton(LCaption, CustomButtonExecute);
        end;
    end);
  end;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
begin
  if Assigned(lbButtons.Selected) and Assigned(FService) then
  begin
    FService.DeleteButton(lbButtons.Selected.Index - 1);// -1 because of group header
    lbButtons.Items.Delete(lbButtons.Selected.Index);
  end;
end;

procedure TForm1.CustomButtonExecute(Sender: TObject);
begin
  if Assigned(Sender) and Sender.InheritsFrom(TVirtualKeyboardToolButton) then
    ShowMessage(Format('Pressed custom toolbutton "%s"', [TVirtualKeyboardToolButton(Sender).Title]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(FService)) then
  begin
    swToolbar.IsChecked := FService.IsToolbarEnabled;
    swDoneButton.IsChecked := FService.IsHideKeyboardButtonVisible;
  end
  else
  begin
    FService := nil;
    swToolbar.Enabled := False;
    lbButtons.Enabled := False;
    swDoneButton.Enabled := False;
    ShowMessage('Virtual keyboard service is not supported');
  end;
end;

procedure TForm1.lbButtonsChange(Sender: TObject);
begin
  btnDelete.Enabled := (lbButtons.Selected <> nil);
end;

procedure TForm1.swDoneButtonSwitch(Sender: TObject);
begin
  if Assigned(FService) then
    FService.SetHideKeyboardButtonVisibility(swDoneButton.IsChecked);
end;

procedure TForm1.swToolbarSwitch(Sender: TObject);
begin
  if Assigned(FService) then
    FService.SetToolbarEnabled(swToolbar.IsChecked);
end;

end.
