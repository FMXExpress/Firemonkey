
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
{******************************************************************************}
{                                                                              }
{                         Delphi FireMonkey Platform                           }
{                                                                              }
{                     Flip View Control for Metropolis UI                      }
{                                                                              }
{                Copyright(c) 2012 Embarcadero Technologies, Inc.              }
{                                                                              }
{                                                                              }
{     This unit containes abstract interface for access and navigate           }
{     to slider data item                                                      }
{                                                                              }
{******************************************************************************}

unit FMX.FlipView.Data;

interface

uses
  System.Generics.Collections, System.Rtti, FMX.Types, FMX.Graphics;

type

{ TAbstractDataSource }

  /// <summary>
  ///   It is collection of abstract data items with access methods.
  /// </summary>
  TAbstractDataSource = class abstract
  protected
    function GetCurrent: TObject; virtual; abstract;
    function GetNext: TObject; virtual; abstract;
    function GetPrevious: TObject; virtual; abstract;
  public
    { Items Navigation }
    procedure GoFirst; virtual; abstract;
    procedure Forward; virtual; abstract;
    procedure Backward; virtual; abstract;
    procedure GoLast; virtual; abstract;
    function IsFirst: Boolean; virtual; abstract;
    function IsLast: Boolean; virtual; abstract;
    function HasItems: Boolean; virtual; abstract;
    { Data Access}
    procedure Load; virtual; abstract;
    procedure Clear; virtual; abstract;
    property Current: TObject read GetCurrent;
    property Next: TObject read GetNext;
    property Previous: TObject read GetPrevious;
  end;

  /// <summary>
  ///   It is collection of images (TBitmap)
  /// </summary>
  TImageDataSource = class (TAbstractDataSource)
  strict private
    FImages: array of TBitmap;
    FCurrentIndex: Integer;
  protected
    function GetCurrent: TObject; override;
    function GetNext: TObject; override;
    function GetPrevious: TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    { Items Navigation }
    function IsFirst: Boolean; override;
    function IsLast: Boolean; override;
    function HasItems: Boolean; override;
    procedure GoFirst; override;
    procedure Forward; override;
    procedure Backward; override;
    procedure GoLast; override;
    { Data Access}
    procedure Clear; override;
    procedure Load; override;
  end;

implementation

uses
  System.SysUtils, Winapi.ActiveX;

{ TImageIterator }

procedure TImageDataSource.Clear;
var
  I: Integer;
begin
  for I := Low(FImages) to High(FImages) do
    FImages[I].Free;
  SetLength(FImages, 0);
end;

constructor TImageDataSource.Create;
begin
  FCurrentIndex := -1;
end;

function TImageDataSource.GetCurrent: TObject;
begin
  Result := FImages[FCurrentIndex];
end;

destructor TImageDataSource.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TImageDataSource.IsFirst: Boolean;
begin
  Result := (FCurrentIndex <> -1) and (FCurrentIndex = Low(FImages));
end;

function TImageDataSource.IsLast: Boolean;
begin
  Result := (FCurrentIndex <> -1) and (FCurrentIndex = High(FImages));
end;

procedure TImageDataSource.Load;
const
  ImagesPath = './images/';
var
  SR: TSearchRec;
  Res: Integer;
  I: Integer;
  ImagesFilter: string;
begin
  I := 0;
  // Find all images
  ImagesFilter := ImagesPath + '*.jpg';
  Res := FindFirst(ImagesFilter, faAnyFile, SR);
  CoInitialize(nil);
  while Res = 0 do
  begin
    SetLength(FImages, I + 1);
    FImages[I] := TBitmap.CreateFromFile(ImagesPath + SR.Name);
    Res := FindNext(SR);
    Inc(I);
  end;
  if Length(FImages) > 0 then
    FCurrentIndex := 0
  else
    FCurrentIndex := -1;
end;

procedure TImageDataSource.Forward;
begin
  if not IsLast then
    Inc(FCurrentIndex);
end;

procedure TImageDataSource.Backward;
begin
  if not IsFirst then
    Inc(FCurrentIndex, -1);
end;

procedure TImageDataSource.GoFirst;
begin
  if Length(FImages) > 0 then
    FCurrentIndex := 0
  else
    FCurrentIndex := -1;
end;

procedure TImageDataSource.GoLast;
begin
  if Length(FImages) > 0 then
    FCurrentIndex := High(FImages)
  else
    FCurrentIndex := -1;
end;

function TImageDataSource.GetNext: TObject;
begin
  if not IsLast then
    Result := FImages[FCurrentIndex + 1];
end;

function TImageDataSource.GetPrevious: TObject;
begin
  if not IsFirst then
    Result := FImages[FCurrentIndex - 1];
end;

function TImageDataSource.HasItems: Boolean;
begin
  Result := Length(FImages) > 0;
end;

end.
