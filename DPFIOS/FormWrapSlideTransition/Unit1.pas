unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UIToolbar,
  DPF.iOS.BaseControl, DPF.iOS.UIView, DPF.iOS.UISegmentedControl,
  DPF.iOS.UIButton, DPF.iOS.UILabel, DPF.iOS.UISwitch, iOSApi.UIKIT,
  DPF.iOS.UITableViewItems, DPF.iOS.UITableView;

type
  TForm1 = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFUITableView1: TDPFUITableView;
    procedure DPFToolbar1BarItems3Click( Sender: TObject );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section: Integer; RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
    procedure SlideAnimation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses uMain, Unit2;

procedure TForm1.DPFToolbar1BarItems3Click( Sender: TObject );
begin
  SlideAnimation;
end;

procedure TForm1.DPFUITableView1ItemSelect( Sender: TObject; Section: Integer; RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  DPFUITableView1.Align := TAlignLayout.alNone;
  Form2.DPFLabel2.Text  := DPFUITableView1.Sections.Items[Section].TableItems.Items[RowNo].ItemText.Text + ' selected!';
  SlideAnimation;
end;

procedure TForm1.SlideAnimation;
begin
  fMain.Frame1.SetBounds( 0, 0, fMain.MainView.Width, fMain.MainView.Height );
  fMain.Frame2.SetBounds( 0, 0, fMain.MainView.Width, fMain.MainView.Height );
  fMain.Frame2.SetBounds( fMain.MainView.Width, 0, fMain.MainView.Width, fMain.MainView.Height );
  fMain.Frame2.Visible := True;
  TUIView.OCClass.beginAnimations( nil, nil );
  TUIView.OCClass.setAnimationDuration( 0.4 );
  fMain.Frame1.Visible := true;
  fMain.Frame1.Form    := Form1;
  fMain.Frame2.Form    := Form2;
  fMain.Frame1.SetBounds( ( -1 * fMain.MainView.WIdth ), 0, fMain.MainView.Width, fMain.MainView.Height );
  fMain.Frame2.SetBounds( 0, 0, fMain.MainView.Width, fMain.MainView.Height );
  TUIView.OCClass.commitAnimations;
end;

end.
