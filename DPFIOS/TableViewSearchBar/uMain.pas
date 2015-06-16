unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UITableView,
  DPF.iOS.UIView,
  DPF.iOS.UISearchBar,
  DPF.iOS.UIButton,
  DPF.iOS.UIViewController,
  DPF.iOS.UINavigationController,
  DPF.iOS.CheckBox,
  DPF.iOS.Keyboard;

type
  TFTableViewSearch = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFSearchBar1: TDPFSearchBar;
    DPFNavigationController1: TDPFNavigationController;
    DPFNavigationControllerPage1: TDPFNavigationControllerPage;
    DPFCheckBox1: TDPFCheckBox;
    DPFKeyboard1: TDPFKeyboard;
    procedure DPFCheckBox1Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFSearchBar1ScopebarClicked( Sender: TObject; SelectedIndex: Integer );
    procedure DPFSearchBar1Changed( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewSearch: TFTableViewSearch;

implementation

{$R *.fmx}
{ TFTableView1 }

procedure TFTableViewSearch.DPFCheckBox1Click( Sender: TObject );
begin
  if DPFCheckBox1.Checked then
    DPFUITableView1.Options.SearchBar.SearchKind := TDPFTableViewSearchKind.skContains
  else
    DPFUITableView1.Options.SearchBar.SearchKind := TDPFTableViewSearchKind.skStartsWith;
end;

procedure TFTableViewSearch.DPFSearchBar1Changed( Sender: TObject );
begin
  DPFNavigationControllerPage1.PageViewTitle := 'Table Search: ' + DPFUITableView1.SearchItemCount.ToString;
end;

procedure TFTableViewSearch.DPFSearchBar1ScopebarClicked( Sender: TObject; SelectedIndex: Integer );
begin
  DPFNavigationControllerPage1.PageViewTitle := DPFSearchBar1.ScopeButtonTitles[SelectedIndex];
  if SelectedIndex = 0 then
    DPFUITableView1.SearchCategory := ''
  else
    DPFUITableView1.SearchCategory := DPFSearchBar1.ScopeButtonTitles[SelectedIndex];
end;

procedure TFTableViewSearch.FormShow( Sender: TObject );
begin
  DPFCheckBox1.Checked                       := DPFUITableView1.Options.SearchBar.SearchKind = TDPFTableViewSearchKind.skContains;
  DPFNavigationControllerPage1.PageViewTitle := DPFSearchBar1.ScopeButtonTitles[DPFSearchBar1.SelectedScopeButtonIndex];
end;

procedure TFTableViewSearch.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
