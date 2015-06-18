{ Minimal Leap Data Structures

  Copyright (C) 2013 Michael Van Canneyt (michael@freepascal.org);

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit leapdata;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$DEFINE USEINLINE}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs;

Const
  DefaultMaxFrames = 100;

Type
  TFloat = Double;
  TLeapID = Integer;

{$IFNDEF FPC}
  TFPList = TList;
{$ENDIF}

  // Forward declarations
  TFrame = Class;
  THand = Class;
  TFinger = Class;
  TPointable = Class;
  TTool = Class;
  TGesture = Class;

  TFrameList = Class;
  THandList = Class;
  TFingerList = Class;
  TPointableList = Class;
  TToolList = Class;
  TGestureList = Class;

  { TPoint }
  T3DPoint = Record
    X,Y,Z : TFloat;
  end;
  T3DVector = T3DPoint;

  T3DMatrixRow = Array[1..3] of TFloat;
  T3DMatrix = Array[1..3] of T3DMatrixRow;

  { TBaseList }

  TBaseList = Class(TFPList)
  Protected
    Procedure ClearObjects;
  end;


  { TIDObject }

  TIDObject = Class(TObject)
  private
    FID: TLeapID;
  Public
    Constructor Create(AID : TLeapID); virtual;
    Function Valid : Boolean; virtual;
    Property ID : TLeapID Read FID Write FID;
  end;

  { TBaseIDList }

  TBaseIDList = Class(TBaseList)
  Public
    Function IndexOfID(AID : TLeapID) : Integer;
    Function FindIDObject(AID : TLeapID) : TIDObject;
  end;

  { TFrameObject }


  TFrameObject = Class(TIDObject)
  private
    FFrame: TFrame;
  Public
    Constructor Create(AFrame : TFrame; AID : TLeapID); virtual;
    Procedure Resolve; virtual;
    Property Frame : TFrame Read FFrame;
  end;


  { TPointable }

  TPointable = Class(TFrameObject)
  private
    FDirection: T3DVector;
    FHand: THand;
    FHandID: TLeapID;
    FLength: TFloat;
    FStabilizedTipPosition: T3DPoint;
    FTipPosition: T3DPoint;
    FTipVelocity: T3DVector;
    FTouchDist: TFloat;
    FTouchZone: String;
    FWidth: TFloat;
  protected
    function GetTool: Boolean; virtual;
    Procedure ResolveHand;
    Procedure Resolve; override;
  Public
    Property Hand : THand Read FHand;
    Property HandID : TLeapID Read FHandID Write FHandID;
    Property Length : TFloat Read FLength Write FLength;
    Property Direction : T3DVector Read FDirection Write FDirection;
    Property TipPosition : T3DPoint Read FTipPosition Write FTipPosition;
    Property StabilizedTipPosition : T3DPoint Read FStabilizedTipPosition Write FStabilizedTipPosition;
    Property TipVelocity : T3DVector Read FTipVelocity Write FTipVelocity;
    Property Tool : Boolean Read GetTool;
    Property Width : TFloat Read FWidth Write FWidth;
    Property TouchDist : TFloat Read FTouchDist Write FTouchDist;
    Property TouchZone : String Read FTouchZone Write FTouchZone;
  end;

  TTool = Class(TPointable);

  { TFinger }

  TFinger = Class(TPointable)
  Protected
    function GetTool: Boolean; override;
  end;

  { THand }

  THand = Class (TFrameObject)
  private
    FDirection: T3DVector;
    FFingers: TFingerList;
    FPalmNormal: T3DVector;
    FPalmPosition: T3DPoint;
    FPalmVelocity: T3DVector;
    FRotation: T3DVector;
    FRotationMatrix: T3DMatrix;
    FScaleFactor: TFLoat;
    FSphereCenter: T3DPoint;
    FSphereRadius: TFloat;
    FTools: TToolList;
  Public
    Destructor Destroy; override;
    Procedure Resolve; override;
    Property Fingers : TFingerList Read FFingers;
    Property Tools : TToolList Read FTools;
    Property Direction : T3DVector Read FDirection Write FDirection;
    Property PalmNormal : T3DVector Read FPalmNormal Write FPalmNormal;
    Property PalmPosition : T3DPoint Read FPalmPosition Write FPalmPosition;
    Property PalmVelocity : T3DVector Read FPalmVelocity Write FPalmVelocity;
    property SphereCenter : T3DPoint Read FSphereCenter Write FSphereCenter;
    property SphereRadius : TFloat Read FSphereRadius Write FSphereRadius;
    Property RotationMatrix : T3DMatrix Read FRotationMatrix Write FRotationMatrix;
    Property Translation : T3DVector Read FRotation Write FRotation;
    Property Scalefactor : TFLoat Read FScaleFactor Write FScaleFactor;
  end;

  TLeapIDArray = Array of TLeapID;

  { THand }

  TGestureType = (gtUnknown,gtKeyTap,gtScreenTap,gtSwipe,gtCircle);
  TGestureState = (gsStart,gsUpdate,gsStop);

  { TGesture }

  TGesture = Class (TFrameObject)
  private
    FDuration: Integer;
    FPointableIDS: TLeapIDArray;
    FHandIDs: TLeapIDArray;
    FHands: THandList;
    FPointables: TPointableList;
    FState: TGestureState;
  Protected
    function GetType: TGestureType; virtual;
    Procedure ResolveHands;
    Procedure ResolvePointables;
  Public
    Destructor Destroy; override;
    Procedure Resolve; override;
    Procedure SetHandIDs(IDS: Array of integer);
    Procedure SetPointableIDs(IDS: Array of integer);
    Procedure AddHand(AID : TLeapID);
    Procedure AddPointable(AID : TLeapID);
    Property HandIDS : TLeapIDArray Read FHandIDs;
    Property PointableIDS : TLeapIDArray Read FPointableIDS;
    Property Hands : THandList Read FHands ;
    Property Pointables : TPointableList Read FPointables;
    Property Duration : Integer Read FDuration Write FDuration;
    Property GestureType : TGestureType Read GetType;
    Property State : TGestureState Read FState Write FState;
  end;
  TGestureClass = Class of TGesture;

  TPositionGesture = Class(TGesture)
  private
    FDirection: T3DVector;
    FPosition: T3DVector;
  Public
    Property Direction : T3DVector Read FDirection Write FDirection;
    Property Position : T3DPoint Read FPosition Write FPosition;
  end;
  { TTapGesture }

  TTapGesture = Class(TPositionGesture)
  Private
    FProgress: TFloat;
  Public
    Property Progress : TFloat Read FProgress Write FProgress;
  end;

  { TKeyTapgesture }

  TKeyTapgesture = Class(TTapgesture)
  Protected
    function GetType: TGestureType; override;
  end;

  { TScreenTapgesture }

  TScreenTapgesture = Class(TTapgesture)
  Protected
    function GetType: TGestureType; override;
  end;

  { TSwipeTapgesture }

  TSwipeGesture = Class(TPositionGesture)
  private
    FSpead: TFloat;
    FStartPosition: T3DPoint;
  Protected
    function GetType: TGestureType; override;
  Public
    Property Speed : TFloat Read FSpead Write FSpead;
    Property StartPosition : T3DPoint Read FStartPosition Write FStartPosition;
  end;

  { TCircleGesture }

  TCircleGesture = Class(TGesture)
  private
    FCenter: T3DPoint;
    FNormal: T3DVector;
    FProgress: TFloat;
    FRadius: TFloat;
  Protected
    function GetType: TGestureType; override;
  Public
    Property Center : T3DPoint Read FCenter Write FCenter;
    Property Normal : T3DVector Read FNormal Write FNormal;
    Property Progress : TFloat Read FProgress Write FProgress;
    Property Radius : TFloat Read FRadius Write FRadius;
  end;

  { THandList }

  THandList = Class(TBaseIDList)
  private
    function GetH(AIndex : Integer): THand;
    procedure SetH(AIndex : Integer; AValue: THand);
  Public
    Function FindHandByID(AID : TLeapID) : THand;
    Property Hands[AIndex : Integer] : THand Read GetH Write SetH; default;
  end;

  { TFingerList }

  TFingerList = Class(TBaseIDList)
  private
    function GetF(AIndex : Integer): TFinger;
    procedure SetF(AIndex : Integer; AValue: TFinger);
  Public
    Function FindFingerByID(AID : TLeapID) : TFinger;
    Property Fingers[AIndex : Integer] : TFinger Read GetF Write SetF; default;
  end;

  { TPointableList }

  TPointableList = Class(TBaseIDList)
  private
    function GetP(AIndex : Integer): TPointable;
    procedure SetP(AIndex : Integer; AValue: TPointable);
  Public
    Function FindPointableByID(AID : TLeapID) : TPointable;
    Property Pointables[AIndex : Integer] : TPointable Read GetP Write SetP; default;
  end;

  { TToolList }

  TToolList = Class(TBaseIDList)
  private
    function GetT(AIndex : Integer): TTool;
    procedure SetT(AIndex : Integer; AValue: TTool);
  Public
    Function FindToolByID(AID : TLeapID) : TTool;
    Property Tools[AIndex : Integer] : TTool Read GetT Write SetT; default;
  end;

  { TGestureList }

  TGestureList = Class(TBaseIDList)
  private
    function GetG(AIndex : Integer): TGesture;
    procedure SetG(AIndex : Integer; AValue: TGesture);
  Public
    Function FindGestureByID(AID : TLeapID) : TGesture;
    Property Gestures[AIndex : Integer] : TGesture Read GetG Write SetG; default;
  end;

  { TInteractionBox }

  TInteractionBox = Class
  private
    FCenter: T3DVector;
    FDimension: T3DVector;
  Public
    Constructor Create(Const ACenter, ADimension : T3DVector);
    Function Normalize(AVector : T3DVector; Clip : Boolean = True) : T3DVector;
    Function DeNormalize(AVector : T3DVector) : T3DVector;
    Property Center : T3DVector Read FCenter Write FCenter;
    Property Dimensions : T3DVector Read FDimension Write FDimension;
    Property Width : TFloat Read FDimension.X Write FDimension.X;
    Property Height : TFloat Read FDimension.Y Write FDimension.Y;
    Property Depth : TFloat Read FDimension.Z Write FDimension.Z;
  end;

  { TFrame }

  TFrame = Class (TIDObject)
  private
    FGestures: TGestureList;
    FInteractionBox: TInteractionBox;
    FObjects : TBaseList;
    FFingers: TFingerList;
    FHands: THandList;
    FPointables: TPointableList;
    FRotation: T3DVector;
    FRotationMatrix: T3DMatrix;
    FScaleFactor: TFLoat;
    FTimeStamp: Int64;
    FTools: TToolList;
    procedure SetInteractionBox(AValue: TInteractionBox);
  Protected
    Procedure AddPointable(APointable : TPointable);
  Public
    Constructor Create(AID : TLeapID); override;
    Destructor Destroy; override;
    Procedure AddFrameObject(AObject : TFrameObject);
    Procedure Resolve;
    Function Hand(AID : TLeapID) : THand;
    Function Finger(AID : TLeapID) : TFinger;
    Function Tool(AID : TLeapID) : TTool;
    Function Pointable(AID : TLeapID) : TPointable;
    Function Gesture(AID : TLeapID) : TGesture;
    Property Hands : THandList Read FHands;
    Property Fingers : TFingerList Read FFingers;
    Property Tools : TToolList Read FTools;
    Property Gestures : TGestureList Read FGestures;
    Property Pointables : TPointableList Read FPointables;
    Property TimeStamp : Int64 Read FTimeStamp Write FTimeStamp;
    Property RotationMatrix : T3DMatrix Read FRotationMatrix Write FRotationMatrix;
    Property Translation : T3DVector Read FRotation Write FRotation;
    Property Scalefactor : TFLoat Read FScaleFactor Write FScaleFactor;
    Property InteractionBox : TInteractionBox Read FInteractionBox Write SetInteractionBox;
  end;
  TFrameClass = Class of TFrame;

  { TFrameList }

  TFrameList = Class(TBaseIDList)
  private
    function GetF(AIndex : Integer): TFrame;
    procedure SetF(AIndex : Integer; AValue: TFrame);
  Public
    Function FindFrameByID(AID : TLeapID) : TFrame;
    Property Frames[AIndex : Integer] : TFrame Read GetF Write SetF; default;
  end;

  { TLeapController }
  TFrameEvent = Procedure(Sender : TObject; AFrame : TFrame) of Object;

  TLeapController = Class(TComponent)
  private
    FEnableGestures: Boolean;
    // Change to circular list in time, for efficiency
    FFrames: TFrameList;
    FLastFrame : TFrame;
    FMaxFrames: Integer;
    FOnFrame: TFrameEvent;
    FOnversion: TNotifyEvent;
    FResolveFrames: Boolean;
    FVersion: Integer;
    function GetF(AIndex : Integer): TFrame;
    function GetFC: Integer;
    procedure SetMaxFrames(AValue: Integer);
  Protected
    Procedure GetLastFrame;
    Procedure SetVersion(V : Integer);
    procedure DoOnFrame(AFrame: TFrame); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function AddFrame(F : TFrame) : Integer;
    Function NewFrame(Const AID : TLeapID) : TFrame;
    Procedure DeleteFrame(F : TFrame); overload;
    Procedure DeleteFrame(I : Integer); overload;
    Function ExtractFrame(I : Integer) : TFrame;
    Function FindFrameByID(AID : TLeapID) : TFrame;
    // Frames are owned by controller. Do not free them, unless you extract them first.
    Property Frames[AIndex :Integer] : TFrame read GetF;
    Property FrameCount : Integer Read GetFC;
    Property LastFrame : TFrame Read FLastFrame;
    Property Version : Integer Read FVersion;
  Published
    // When doing an add, the frame is resolved.
    Property ResolveFrames : Boolean Read FResolveFrames Write FResolveFrames;
    Property EnableGestures : Boolean Read FEnableGestures Write FEnableGestures;
    Property MaxFrames : Integer Read FMaxFrames Write SetMaxFrames;
    Property OnFrame : TFrameEvent Read FOnFrame Write FOnFrame;
    Property OnVersion : TNotifyEvent Read FOnversion Write FonVersion;
  end;

  ELeap = Class(Exception);

Function Get3DPoint(X,Y,Z : TFloat) : T3DPoint;{$IFDEF USEINLINE}inline;{$ENDIF}
Function Get3DVector(X,Y,Z : TFloat) : T3DVector;{$IFDEF USEINLINE}inline;{$ENDIF}
Function Get3DRow (C1,C2,C3 : TFloat) : T3DMatrixRow; overload;{$IFDEF USEINLINE}inline;{$ENDIF}
Function Get3DRow (R : Array of TFloat) : T3DMatrixRow; overload;{$IFDEF USEINLINE}inline;{$ENDIF}
Function Get3DMatrix (R1,R2,R3 : T3DMatrixRow) : T3DMatrix;overload;{$IFDEF USEINLINE}inline;{$ENDIF}
Function Get3DMatrix (R1,R2,R3 : Array of TFloat) : T3DMatrix;overload;{$IFDEF USEINLINE}inline;{$ENDIF}

Function VectorLength(P : T3DPoint) : TFloat; {$IFDEF USEINLINE}inline;{$ENDIF}
Function LengthSquared(P : T3DPoint) : TFloat; {$IFDEF USEINLINE}inline;{$ENDIF}
Function Dot(P1,P2 : T3DPoint) : TFloat; {$IFDEF USEINLINE}inline;{$ENDIF}
Function AngleTo(P1,P2 : T3DPoint) : TFloat;{$IFDEF USEINLINE}inline;{$ENDIF}

implementation
uses math;

Function LengthSquared(P : T3DPoint) : TFloat;

begin
  Result:=Sqr(P.X)+Sqr(P.Y)+Sqr(P.Z);
end;

Function VectorLength(P : T3DPoint) : TFloat;{$IFDEF USEINLINE}inline;{$ENDIF}

begin
  Result:=Sqrt(LengthSquared(P));
end;

Function Dot(P1,P2 : T3DPoint) : TFloat; {$IFDEF USEINLINE}inline;{$ENDIF}

begin
  Result:=P1.X*P2.X + P1.Y*P2.Y + P1.Z*P2.Z;
end;

function AngleTo(P1, P2: T3DPoint): TFloat;
Var
  D : TFloat;
begin
  D:=LengthSquared(P1)*LengthSquared(P2);
  if (D<=0.0) then
    Result:=0
  else
    Result:=arccos(dot(P1,P2)/Sqrt(D));
end;

function Get3DPoint(X, Y, Z: TFloat): T3DPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=Z;
end;

function Get3DVector(X, Y, Z: TFloat): T3DVector;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=Z;
end;

function Get3DRow(C1, C2, C3: TFloat): T3DMatrixRow;
begin
  Result[1]:=C1;
  Result[2]:=C2;
  Result[3]:=C3;
end;

function Get3DRow(R: array of TFloat): T3DMatrixRow;
begin
  If High(R)<>2 then
    Raise ELeap.Create('Not a valid 3D row, count must be 3');
  Result:=Get3DRow(R[0],R[1],R[2]);
end;

function Get3DMatrix(R1, R2, R3: T3DMatrixRow): T3DMatrix;
begin
  Result[1]:=R1;
  Result[2]:=R2;
  Result[3]:=R3;
end;

function Get3DMatrix(R1, R2, R3: array of TFloat): T3DMatrix;
begin
  Result:=Get3DMatrix(Get3DRow(R1),Get3DRow(R2),Get3DRow(R3));
end;

{ TGestureList }

function TGestureList.GetG(AIndex: Integer): TGesture;
begin
  Result:=TGesture(Items[AIndex]);
end;

procedure TGestureList.SetG(AIndex: Integer; AValue: TGesture);
begin
  Items[AIndex]:=Avalue;
end;

function TGestureList.FindGestureByID(AID: TLeapID): TGesture;

Var
  I : Integer;

begin
  I:=IndexOfID(AID);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetG(I);
end;

{ TCircleGesture }

function TCircleGesture.GetType: TGestureType;
begin
  Result:=gtCircle;
end;

{ TSwipeTapgesture }

function TSwipeGesture.GetType: TGestureType;
begin
  result:=gtswipe;
end;

{ TScreenTapgesture }

function TScreenTapgesture.GetType: TGestureType;
begin
  Result:=gtScreenTap;
end;

{ TKeyTapgesture }

function TKeyTapgesture.GetType: TGestureType;
begin
  result:=gtKeyTap;
end;


{ TGesture }

function TGesture.GetType: TGestureType;
begin
  Result:=gtUnknown;
end;

procedure TGesture.ResolveHands;

Var
  I : integer;

begin
  if Frame=Nil then
    Exit;
  If (FHands=Nil) then
    FHands:=THandList.Create;
  FHands.Clear;
  For I:=0 to Length(FHandIDs)-1 do
    FHands.Add(Frame.Hands.FindHandByID(FHandIDs[i]));
end;

procedure TGesture.ResolvePointables;

Var
  I : integer;
begin
  if Frame=Nil then
    Exit;
  If (FPointables=Nil) then
    FPointables:=TPointableList.Create;
  FPointables.Clear;
  For I:=0 to Length(FPointableIDs)-1 do
    FPointables.Add(Frame.Pointables.FindPointableByID(FPointableIDs[i]));
end;

destructor TGesture.Destroy;
begin
  FreeAndNil(FHands);
  FreeAndNil(FPointables);
  inherited Destroy;
end;

procedure TGesture.Resolve;
begin
  inherited Resolve;
  ResolveHands;
  ResolvePointables;
end;

procedure TGesture.SetHandIDs(IDS: array of integer);

Var
  I : Integer;

begin
  SetLength(FHandIDS,High(IDS)+1);
  For I:=0 to High(IDS) do
    FHandIDS[I]:=IDS[i];
end;

procedure TGesture.SetPointableIDs(IDS: array of integer);
Var
  I : Integer;

begin
  SetLength(FPointableIDS,High(IDS)+1);
  For I:=0 to High(IDS) do
    FPointableIDS[I]:=IDS[i];
end;

procedure TGesture.Addhand(AID: TLeapID);
begin
  SetLength(FHandIDS,Length(FHandIDS)+1);
  FHandIDS[Length(FHandIDS)-1]:=AID;
end;

procedure TGesture.AddPointable(AID: TLeapID);
begin
  SetLength(FPointableIDS,Length(FPointableIDS)+1);
  FPointableIDS[Length(FPointableIDS)-1]:=AID;
end;

{ TInteractionBox }

constructor TInteractionBox.Create(const ACenter, ADimension: T3DVector);
begin
  FCenter:=ACenter;
  FDimension:=ADimension;
end;

function TInteractionBox.Normalize(AVector: T3DVector; Clip: Boolean): T3DVector;

  Procedure MM(Var F : TFloat);

  begin
    If F<0 then
      F:=0
    else if F>1 then
      F:=1;
  end;

begin
  Result.X:=0.5+(AVector.X - Center.X)/Width;
  Result.Y:=0.5+(AVector.Y - Center.Y)/Height;
  Result.Z:=0.5+(AVector.X - Center.X)/Depth;
  if Clip then
    begin
    MM(Result.X);
    MM(Result.Y);
    MM(Result.Z);
    end;
end;

function TInteractionBox.DeNormalize(AVector: T3DVector): T3DVector;
begin
  Result.X:=(AVector.X - 0.5) * Width + center.X;
  Result.Y:=(AVector.Y - 0.5) * Height + center.Y;
  Result.Z:=(AVector.Z - 0.5) * Depth + center.Z;
end;

{ TFrameList }

function TFrameList.GetF(AIndex : Integer): TFrame;
begin
  Result:=TFrame(Items[AIndex]);
end;

procedure TFrameList.SetF(AIndex : Integer; AValue: TFrame);
begin
  Items[AIndex]:=TFrame;
end;

function TFrameList.FindFrameByID(AID: TLeapID): TFrame;
begin
  Result:=TFrame(FindIDObject(AID));
end;

{ TLeapController }

function TLeapController.GetF(AIndex : Integer): TFrame;
begin
  Result:=FFrames[AIndex];
end;

function TLeapController.GetFC: Integer;
begin
  Result:=FFrames.Count;
end;

procedure TLeapController.SetMaxFrames(AValue: Integer);
begin
  if FMaxFrames=AValue then Exit;
  While FrameCount>AValue do
    DeleteFrame(0);
  FMaxFrames:=AValue;
  GetLastFrame;
end;

procedure TLeapController.GetLastFrame;
begin
  if FFrames.Count>0 then
    FLastFrame:=FFrames[FFrames.Count-1]
  else
    FLastFrame:=Nil;
end;

procedure TLeapController.SetVersion(V: Integer);
begin
  FVersion:=V;
  if Assigned(FOnversion) then
    FOnversion(Self);
end;

constructor TLeapController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrames:=TFrameList.Create;
  MaxFrames:=DefaultMaxFrames;
end;

destructor TLeapController.Destroy;
begin
  FFrames.ClearObjects;
  FreeAndNil(FFrames);
  inherited Destroy;
end;

function TLeapController.AddFrame(F: TFrame): Integer;
begin
   While FrameCount>=FMaxFrames do
     DeleteFrame(0);
   Result:=FFrames.Add(F);
   FLastFrame:=F;
   try
     F.Resolve;
     DoOnFrame(F);
   except

   end;
end;

Procedure TLeapController.DoOnFrame(AFrame : TFrame);

begin
  If Assigned(FonFrame) then
    FonFrame(Self,AFrame);
end;

function TLeapController.NewFrame(Const AID : TLeapID): TFrame;
begin
  Result:=TFrame.Create(AID);
  AddFrame(Result);
end;

procedure TLeapController.DeleteFrame(F: TFrame);
begin
  FFrames.Remove(F);
  F.Free;
  GetLastFrame;
end;

procedure TLeapController.DeleteFrame(I: Integer);

begin
  FFrames[i].Free;
  FFrames.Delete(i);
  GetLastFrame;
end;

function TLeapController.ExtractFrame(I: Integer): TFrame;
begin
  Result:=FFrames[i];
  FFrames.Delete(i);
  GetLastFrame;
end;

function TLeapController.FindFrameByID(AID: TLeapID): TFrame;
begin
  Result:=FFrames.FindFrameByID(AID);
end;

{ TIDObject }

constructor TIDObject.Create(AID: TLeapID);
begin
  FID:=AID;
end;

function TIDObject.Valid: Boolean;
begin
  Result:=True;
end;

{ TToolList }

function TToolList.GetT(AIndex: Integer): TTool;
begin
  Result:=TTool(Items[AIndex]);
end;

procedure TToolList.SetT(AIndex: Integer; AValue: TTool);
begin
  Items[AIndex]:=AValue;
end;

function TToolList.FindToolByID(AID: TLeapID): TTool;
begin
  Result:=TTool(FindIDObject(AID));
end;

{ TPointableList }

function TPointableList.GetP(AIndex: Integer): TPointable;
begin
  Result:=TPointable(Items[AIndex])
end;

procedure TPointableList.SetP(AIndex: Integer; AValue: TPointable);
begin
  Items[AIndex]:=AValue;
end;

function TPointableList.FindPointableByID(AID: TLeapID): TPointable;

begin
  Result:=TPointable(FindIDObject(AID));
end;

{ TFrame }

procedure TFrame.SetInteractionBox(AValue: TInteractionBox);
begin
  if FInteractionBox=AValue then Exit;
  FreeAndNil(FInteractionBox);
  FInteractionBox:=AValue;
end;

procedure TFrame.AddPointable(APointable: TPointable);
begin
  FPointables.Add(APointable);
  if APointable is TTool then
    FTools.Add(APointable)
  else if APointable is TFinger then
    FFingers.Add(APointable);
end;

constructor TFrame.Create(AID: TLeapID);
begin
  inherited Create(AID);
  FObjects:=TBaseList.Create();
  FTools:=TToolList.Create;
  FHands:=THandList.Create;
  FGestures:=TGestureList.Create;
  FFingers:=TFingerList.Create;
  FPointables:=TPointableList.Create;
end;

destructor TFrame.Destroy;
begin
  FObjects.ClearObjects;
  FreeAndNil(FObjects);
  FreeAndNil(FPointables);
  FreeAndNil(FTools);
  FreeAndNil(FHands);
  FreeAndNil(FGEstures);
  FreeAndNil(FFingers);
  FreeAndNil(FInteractionBox);
  inherited Destroy;
end;

procedure TFrame.AddFrameObject(AObject: TFrameObject);
begin
  FObjects.Add(AObject);
  If AObject is THand then
    FHands.Add(AObject)
  else if AObject is TPointable then
    AddPointable(TPointable(AObject))
  else if AObject is TGesture then
    FGestures.Add(AObject);
end;

procedure TFrame.Resolve;

Var
  I : Integer;

begin
  For I:=0 to FObjects.Count-1 do
    TFrameObject(FObjects[i]).Resolve;
end;

function TFrame.Hand(AID: TLeapID): THand;
begin
  Result:=Hands.FindHandByID(AID);
end;

function TFrame.Finger(AID: TLeapID): TFinger;
begin
  Result:=Fingers.FindFingerByID(AID);
end;

function TFrame.Tool(AID: TLeapID): TTool;
begin
  Result:=Tools.FindToolByID(AID);
end;

function TFrame.Pointable(AID: TLeapID): TPointable;
begin
  Result:=Pointables.FindPointableByID(AID);
end;

function TFrame.Gesture(AID: TLeapID): TGesture;
begin

end;

{ TFingerList }

function TFingerList.GetF(AIndex: Integer): TFinger;
begin
  Result:=TFinger(Items[AIndex]);
end;

procedure TFingerList.SetF(AIndex: Integer; AValue: TFinger);
begin
  Items[AIndex]:=AValue;
end;

function TFingerList.FindFingerByID(AID: TLeapID): TFinger;

begin
   Result:=TFinger(FindIDObject(AID));
end;

{ TBaseIDList }

function TBaseIDList.IndexOfID(AID: TLeapID): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (TIDObject(Items[Result]).ID<>AID) do
    Dec(Result);
end;

function TBaseIDList.FindIDObject(AID: TLeapID): TIDObject;

Var
  I : Integer;

begin
  I:=IndexOfID(AID);
  if (I=-1) then
    Result:=Nil
  else
    Result:=TIDObject(Items[I])
end;

{ TFinger }

function TFinger.GetTool: Boolean;
begin
  Result:=False;
end;

{ TPointable }

function TPointable.GetTool: Boolean;
begin
  Result:=True;
end;

procedure TPointable.ResolveHand;
begin
  if Frame=Nil then
    FHand:=Nil
  else
    FHand:=Frame.FHands.FindHandByID(HandID);
end;

procedure TPointable.Resolve;
begin
  inherited Resolve;
  ResolveHand;
end;

{ TFrameIDObject }

constructor TFrameObject.Create(AFrame: TFrame; AID: TLeapID);
begin
  Inherited Create(AID);
  FFrame:=AFrame;
  If Assigned(FFrame) then
    FFrame.AddFrameObject(Self);
end;

procedure TFrameObject.Resolve;
begin
   // Do nothing;
end;

{ THand }

destructor THand.Destroy;
begin
  FreeAndNil(FTools);
  FreeAndNil(FFingers);
  inherited Destroy;
end;

procedure THand.Resolve;

Var
  I : Integer;

begin
  Inherited;
  if not Assigned(Frame) then exit;
  if Frame.Fingers.Count>0 then
    begin
    FFingers:=TFingerList.Create;
    For I:=0 to Frame.Fingers.Count-1 do
      if (Frame.Fingers[i].HandID=ID) then
        FFingers.Add(Frame.Fingers[i]);
    end;
  if Frame.Tools.Count>0 then
    begin
    FTools:=TToolList.Create;
    For I:=0 to Frame.Tools.Count-1 do
      if (Frame.Tools[i].HandID=ID) then
        FTools.Add(Frame.Tools[i]);
    end;
end;

{ THandList }

function THandList.GetH(AIndex : Integer): THand;
begin
  Result:=THand(Items[AIndex]);
end;

procedure THandList.SetH(AIndex : Integer; AValue: THand);
begin
  Items[AIndex]:=AValue;
end;

function THandList.FindHandByID(AID: TLeapID): THand;

begin
  Result:=THand(FindIDObject(AID));
end;

{ TBaseList }

procedure TBaseList.ClearObjects;

Var
  I : integer;

begin
  For I:=0 to Count-1 do
    TObject(Items[i]).Free;
  Clear;
end;

end.

