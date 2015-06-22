unit uTestBed;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  UPhysics2DTypes, UPhysics2D, uDebugDrawer;

const
  k_maxContactPoints = 2048;

  DefaultStep = 1 / 60;
  velocityIterations = 8;
  positionIterations = 3;

type
  // this is here to remove form class dependency from TTester class
  ITesterForm = interface
    procedure UpdateGravityText(AGravityTextX, AGravityTextY: string);
    procedure ResetTest;
  end;

  TContactPoint = record
    fixtureA, fixtureB: Tb2Fixture;
    normal, position: TVector2;
    id: Tb2ContactID;
    state: Tb2PointState;
    normalImpulse: PhysicsFloat;
    tangentImpulse: PhysicsFloat;
    separation: PhysicsFloat;
  end;

  TSettings = record
    drawShapes,
    drawJoints,
    drawAABBs,
    drawPairs,
    drawContactPoints,
    drawContactNormals,
    drawContactImpulse,
    drawFrictionImpulse,
    drawCOMs,
    drawStats,
    drawKeyInfo,

    enableSleep,
    enableWarmStarting,
    enableContinuousPhysics,
    enableSubStepping,

    pause,
    singleStep,
    realTime,
    customedStep: Boolean;
  end;

  TTester = class;

  TDestructionListener = class(Tb2DestructionListener)
  public
    test: TTester;
    procedure SayGoodbye(fixture: Tb2Fixture); overload; override;
    procedure SayGoodbye(joint: Tb2Joint); overload; override;
  end;

  TTestClass = class of TTester;

  TTester = class(Tb2ContactListener)
  protected
    FTesterForm: ITesterForm;
    m_RemainTime: PhysicsFloat;
  public
    m_stepCount: Integer;
    m_groundBody: Tb2Body;
    m_worldAABB: Tb2AABB;
    m_points: array[0..k_maxContactPoints - 1] of TContactPoint;
    m_pointCount: Int32;
    m_destructionListener: TDestructionListener;
    m_world: Tb2World;
    m_bomb: Tb2Body;
    m_mouseJoint: Tb2MouseJoint;
    m_bombSpawnPoint: TVector2;
    m_bombSpawning: Boolean;
    m_mouseWorld: TVector2;
    m_textLine: Int32;
    m_debugDrawer: TDebugDrawer;

    m_InvertedY: boolean;
    m_DrawDebugInfo: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure NextLine;
    procedure Step(var settings: TSettings; timeStep: PhysicsFloat); virtual;
    procedure Keyboard(key: Byte); virtual;
    procedure KeyboardUp(key: Byte); virtual;
    procedure MouseDown(const p: TVector2); virtual;
    procedure ShiftMouseDown(const p: TVector2);
    procedure MouseUp(const p: TVector2); virtual;
    procedure MouseMove(const p: TVector2);
    procedure LaunchBomb(velocity_factor: PhysicsFloat = 1.0); overload; virtual;
    procedure LaunchBomb(const position, velocity: TVector2); overload;
    procedure SpawnBomb(const worldPt: TVector2);
    procedure CompleteBombSpawn(const p: TVector2);
    procedure SetCanvasTranslation(x, y: PhysicsFloat);
    procedure SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
    procedure DrawText(const text: string);

    // Let derived tests know that a joint was destroyed.
    procedure JointDestroyed(joint: Tb2Joint); virtual;

    // Callbacks for derived classes.
    procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;

  public
    procedure ShiftOrigin(const newOrigin: TVector2);

    procedure UpdateGravityText;
    procedure SetDebugDrawer(const Value: TDebugDrawer);
    procedure SetTesterForm(const Value: ITesterForm);
  end;

type
  PTestEntry = ^TTestEntry;
  TTestEntry = record
    Name: String;
    ClassType: TTestClass;
  end;

procedure RegisterTestEntry(name: String; ClassType: TTestClass);

var
  Test: TTester;

var
  Settings: TSettings;
  ActiveEntry: PTestEntry;
  TestCount: Integer;
  TestEntries: array of TTestEntry;
  ActiveEntryIndex: Integer;

implementation

uses
  System.SysUtils, System.UITypes;

procedure RegisterTestEntry(name: String; ClassType: TTestClass);
var
  i: Integer;
  found: Int32;
begin
  found := TestCount;
  for i := 0 to TestCount - 1 do // Sort by name
    if CompareText(name, TestEntries[i].Name) < 0 then
      begin
        found := i;
        Break;
      end;

  SetLength(TestEntries, TestCount + 1);
  if found < TestCount then
    for i := TestCount downto found + 1 do
      TestEntries[i] := TestEntries[i - 1];

  TestEntries[found].Name := name;
  TestEntries[found].ClassType := ClassType;
  Inc(TestCount);
end;

{ TDestructionListener }

procedure TDestructionListener.SayGoodbye(fixture: Tb2Fixture);
begin
end;

procedure TDestructionListener.SayGoodbye(joint: Tb2Joint);
begin
  if test.m_mouseJoint = joint then
    test.m_mouseJoint := nil
  else
    test.JointDestroyed(joint);
end;

{ TTester }

constructor TTester.Create;
const
  WorldLowerBound: TVector2 = (x: -200.0; y: -100.0);
  WorldUpperBound: TVector2 = (x: 200.0; y: 200.0);
var
  gravity: TVector2;
  bodyDef: Tb2BodyDef;
begin
  m_DrawDebugInfo := True;

  m_InvertedY := True;

  gravity.x := 0.0;
  gravity.y := -10.0;

  m_world := Tb2World.Create(gravity);

   //   UpdateGravityText;  // called after the constructor

  m_RemainTime := 0.0;

  m_worldAABB.lowerBound := WorldLowerBound;
  m_worldAABB.upperBound := WorldUpperBound;

  m_bomb := nil;
  m_textLine := 30;

  m_mouseJoint := nil;
  m_pointCount := 0;

  m_destructionListener := TDestructionListener.Create;
  m_destructionListener.test := Self;
  m_world.DestructionListener := m_destructionListener;
  m_world.SetContactListener(Self);

  m_bombSpawning := False;
  m_stepCount := 0;
  bodyDef := Tb2BodyDef.Create;
  m_groundBody := m_world.CreateBody(bodyDef);

  m_world.WarmStarting := Settings.enableWarmStarting;
  m_world.ContinuousPhysics := Settings.enableContinuousPhysics;
end;

destructor TTester.Destroy;
begin
  m_world.Free;
  m_destructionListener.Free;
  inherited;
end;

procedure TTester.NextLine;
begin
  if m_InvertedY then
    m_TextLine := m_TextLine - 15
  else
    m_TextLine := m_TextLine + 15;
end;

procedure TTester.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   k_impulseScale = 0.1;
   k_axisScale = 0.4;
   clPoint: RGBA = (0.0, 1.0, 0.0, 1.0);
   clLine: RGBA = (0.8, 0.8, 0.8, 1.0);
   clAdd: RGBA = (0.3, 0.95, 0.3, 1.0);
   clPersist: RGBA = (0.3, 0.3, 0.95, 1.0);
   clContactNormal: RGBA = (0.9, 0.9, 0.9, 1.0);
   clContactImpulse: RGBA = (0.9, 0.9, 0.3, 1.0);
   clFrictionImpulse: RGBA = (0.9, 0.2, 0.2, 1.0);
   clBomb: RGBA = (0.0, 0.0, 1.0, 1.0);

var
   i: Integer;
   p1, p2: TVector2;
   cp: TContactPoint;
begin
   if not Settings.customedStep then
   begin
      m_pointCount := 0;
      if not settings.realTime then
         timeStep := DefaultStep;

      if settings.pause then
      begin
         m_RemainTime := 0.0;
         if settings.singleStep then
         begin
            m_world.Step(DefaultStep, velocityIterations, positionIterations, True);
            settings.singleStep := False;
            Inc(m_stepCount);
         end
         else
            m_world.Step(0, 8, 3, True);

         if m_DrawDebugInfo then
         begin
           m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.Red);
           DrawText('**** PAUSED ****');
           m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.White);
         end;
      end
      else
      begin
         if settings.realTime then // Make sure that every frame is processed using a time step of 1/60s.
         begin
            timeStep := timeStep + m_RemainTime;
            while timeStep > DefaultStep do
            begin
               m_world.Step(DefaultStep, velocityIterations, positionIterations);
               timeStep := timeStep - DefaultStep;
            end;
            m_RemainTime := timeStep;
            m_world.DrawDebugData;
         end
         else
            m_world.Step(timeStep, velocityIterations, positionIterations, True);

         Inc(m_stepCount);
      end;
   end;

   if m_DrawDebugInfo then
   begin
     if settings.drawKeyInfo then
     begin
       m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.Red);
       DrawText('Space: Launch bomb   Home: Reset view');
       DrawText('Arrows: Move view   Ctrl & Arrows: Move world');
       DrawText('Right Mouse: Span   Wheel: Scale');
       DrawText('Hold Shift and drag the mouse to spawn a bullet.');
       m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.White);
     end;

     if settings.drawStats then
     begin
       m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.Lime);

       NextLine; // space line
       DrawText(Format('bodies/contacts/joints = %d/%d/%d',
         [m_world.GetBodyCount, m_world.GetContactCount, m_world.GetJointCount]));
       DrawText(Format('proxies/height/balance/quality = %d/%d/%d/%f',
         [m_world.GetProxyCount, m_world.GetTreeHeight, m_world.GetTreeBalance, m_world.GetTreeQuality]));
       NextLine; // space line

       m_debugDrawer.SetDefaultFontColor(TAlphaColorRec.White);
     end;
   end;

   if Assigned(m_mouseJoint) then
   begin
     p1 := m_mouseJoint.GetAnchorB;
     p2 := m_mouseJoint.GetTarget;

     m_debugDrawer.DrawPoint(p1, 4.0, clPoint);
     m_debugDrawer.DrawPoint(p2, 4.0, clPoint);
     m_world.Draw.DrawSegment(p1, p2, clLine);
   end;

   if m_bombSpawning then
   begin
     m_debugDrawer.DrawPoint(m_bombSpawnPoint, 4.0, clBomb);
     m_world.Draw.DrawSegment(m_mouseWorld, m_bombSpawnPoint, clLine);
   end;

   if settings.drawContactPoints then
   begin
     for i := 0 to m_pointCount - 1 do
     begin
       cp := m_points[i];

       if cp.state = b2_addState then // Add
         m_debugDrawer.DrawPoint(cp.position, 10.0, clAdd)
       else if cp.state = b2_persistState then // Persist
         m_debugDrawer.DrawPoint(cp.position, 5.0, clPersist);

       if settings.drawContactNormals then
       {$IFDEF OP_OVERLOAD}
         m_world.Draw.DrawSegment(cp.position, cp.position + k_axisScale * cp.normal, clContactNormal)
       {$ELSE}
         m_world.Draw.DrawSegment(cp.position, Add(cp.position, Multiply(cp.normal, k_axisScale)), clContactNormal)
       {$ENDIF}

       else if settings.drawContactImpulse then
       {$IFDEF OP_OVERLOAD}
         m_debugDrawer.DrawSegment(cp.position, cp.position + k_impulseScale * cp.normalImpulse * cp.normal, clContactImpulse)
       {$ELSE}
         m_debugDrawer.DrawSegment(cp.position, Add(cp.position, Multiply(cp.normal, k_impulseScale * cp.normalImpulse)), clContactImpulse)
       {$ENDIF}

       else if settings.drawFrictionImpulse then
       {$IFDEF OP_OVERLOAD}
         m_debugDrawer.DrawSegment(cp.position, cp.position + k_impulseScale * cp.tangentImpulse * b2Cross(cp.normal, 1.0), clFrictionImpulse);
       {$ELSE}
         m_debugDrawer.DrawSegment(cp.position, Add(cp.position, Multiply(b2Cross(cp.normal, 1.0), k_impulseScale * cp.tangentImpulse)), clFrictionImpulse);
       {$ENDIF}
     end;
   end;
end;

procedure TTester.Keyboard(key: Byte);
begin
end;

procedure TTester.KeyboardUp(key: Byte);
begin
end;

type
   TQueryCallback = class(Tb2QueryCallback)
   public
      m_point: TVector2;
      m_fixture: Tb2Fixture;

      procedure Initialize(const point: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function ReportFixture(fixture: Tb2Fixture): Boolean; override;
   end;

procedure TQueryCallback.Initialize(const point: TVector2);
begin
   m_point := point;
   m_fixture := nil;
end;

function TQueryCallback.ReportFixture(fixture: Tb2Fixture): Boolean;
begin
   if fixture.GetBody.GetType = b2_dynamicBody then
   begin
      if fixture.TestPoint(m_point) then
      begin
         m_fixture := fixture;
         // We are done, terminate the query.
         Result := False;
         Exit;
      end;
   end;

   // Continue the query.
   Result := True;
end;

var
   _QueryCallback: TQueryCallback;

procedure TTester.MouseDown(const p: TVector2);
const
   k_maxCount = 10;
var
   aabb: Tb2AABB;
   d: TVector2;
   body: Tb2Body;
   md: Tb2MouseJointDef;
begin
   m_mouseWorld := p;
   if Assigned(m_mouseJoint) then
      Exit;

   // Make a small box.
   {$IFDEF OP_OVERLOAD}
   d.SetValue(0.001, 0.001);
   aabb.lowerBound := p - d;
   aabb.upperBound := p + d;
   {$ELSE}
   SetValue(d, 0.001, 0.001);
   aabb.lowerBound := Subtract(p, d);
   aabb.upperBound := Add(p, d);
   {$ENDIF}

   // Query the world for overlapping shapes.
   _QueryCallback.Initialize(p);
   m_world.QueryAABB(_QueryCallback, aabb);

   if Assigned(_QueryCallback.m_fixture) then
   begin
      body := _QueryCallback.m_fixture.GetBody;
      md := Tb2MouseJointDef.Create;
      md.bodyA := m_groundBody;
      md.bodyB := body;
      md.target := p;
      md.maxForce := 1000.0 * body.GetMass;
      m_mouseJoint := Tb2MouseJoint(m_world.CreateJoint(md));
      body.SetAwake(True);
   end;
end;

procedure TTester.ShiftMouseDown(const p: TVector2);
begin
  m_mouseWorld := p;
  if Assigned(m_mouseJoint) then
    Exit;

  SpawnBomb(p);
end;

procedure TTester.MouseUp(const p: TVector2);
begin
   if Assigned(m_mouseJoint) then
   begin
      m_world.DestroyJoint(m_mouseJoint);
      m_mouseJoint := nil;
   end;

   if m_bombSpawning then
      CompleteBombSpawn(p);
end;

procedure TTester.MouseMove(const p: TVector2);
begin
   m_mouseWorld := p;
	 if Assigned(m_mouseJoint) then
      m_mouseJoint.SetTarget(p);
end;

procedure TTester.LaunchBomb(velocity_factor: PhysicsFloat = 1.0);
var
   p, v: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
	 p.SetValue(RandomFloat(-15, 15), 30.0);
	 v := -5.0 * velocity_factor * p;
   {$ELSE}
	 SetValue(p, RandomFloat(-15, 15), 30.0);
	 v := Multiply(p, -5.0 * velocity_factor);
   {$ENDIF}
	 LaunchBomb(p, v);
end;

procedure TTester.LaunchBomb(const position, velocity: TVector2);
var
   bd: Tb2BodyDef;
   circle: Tb2CircleShape;
   fd: Tb2FixtureDef;
begin
   if Assigned(m_bomb) then
   begin
      m_world.DestroyBody(m_bomb);
   {$IFNDEF AUTOREFCOUNT}
      m_bomb := nil;
   {$ENDIF}
  end;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   bd.position := position;
   bd.bullet := True;
   m_bomb := m_world.CreateBody(bd);
   m_bomb.SetLinearVelocity(velocity);

   circle := Tb2CircleShape.Create;
   circle.m_radius := 0.3;

   fd := Tb2FixtureDef.Create;
   fd.shape := circle;
   fd.density := 20.0;
   fd.restitution := 0.0;

   {b2AABB aabb;
   aabb.lowerBound := position - b2Vec2(0.3f,0.3f);
   aabb.upperBound := position + b2Vec2(0.3f,0.3f);     }
   m_bomb.CreateFixture(fd);
end;

procedure TTester.SpawnBomb(const worldPt: TVector2);
begin
  m_bombSpawnPoint := worldPt;
  m_bombSpawning := True;
end;

procedure TTester.CompleteBombSpawn(const p: TVector2);
const
  multiplier = 30.0;
var
  vel: TVector2;
begin
  if not m_bombSpawning then
    Exit;

   {$IFDEF OP_OVERLOAD}
   vel := m_bombSpawnPoint - p;
   vel.MultiplyBy(multiplier);
   {$ELSE}
   vel := Subtract(m_bombSpawnPoint, p);
   MultiplyBy(vel, multiplier);
   {$ENDIF}
   LaunchBomb(m_bombSpawnPoint, vel);
   m_bombSpawning := False;
end;

procedure TTester.SetCanvasTranslation(x, y: PhysicsFloat);
begin
  if Assigned(m_debugDrawer) then
    m_debugDrawer.SetCanvasTranslation(x,y);
end;

procedure TTester.SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
begin
  if Assigned(m_debugDrawer) then
    m_debugDrawer.SetCanvasTranslationOffset(dx,dy);
end;

procedure TTester.SetDebugDrawer(const Value: TDebugDrawer);
begin
  m_debugDrawer := Value;
  m_world.Draw := Value;
end;

procedure TTester.SetTesterForm(const Value: ITesterForm);
begin
  FTesterForm := Value;
end;

procedure TTester.DrawText(const text: string);
begin
  if m_DrawDebugInfo then
  begin
    if Assigned(m_debugDrawer) then
      m_debugDrawer.TextOutASCII(text, 5, m_textLine);
    NextLine;
  end;
end;

procedure TTester.JointDestroyed(joint: Tb2Joint);
begin
end;

procedure TTester.PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold);
var
  i: Integer;
  state1, state2: Tb2PointStateArray;
  worldManifold: Tb2WorldManifold;
begin
  if contact.m_manifold.pointCount = 0 then
    Exit;

  b2GetPointStates(state1, state2, oldManifold, contact.m_manifold);

  {$IFDEF OP_OVERLOAD}
  contact.GetWorldManifold(worldManifold);
  {$ELSE}
  GetWorldManifold(contact, worldManifold);
  {$ENDIF}

  i := 0;
  while (i < contact.m_manifold.pointCount) and (m_pointCount < k_maxContactPoints) do
  begin
    m_points[m_pointCount].fixtureA := contact.m_fixtureA;
    m_points[m_pointCount].fixtureB := contact.m_fixtureB;
    m_points[m_pointCount].position := worldManifold.points[i];
    m_points[m_pointCount].normal := worldManifold.normal;
    m_points[m_pointCount].state := state2[i];
    m_points[m_pointCount].normalImpulse := contact.m_manifold.points[i].normalImpulse;
    m_points[m_pointCount].tangentImpulse := contact.m_manifold.points[i].tangentImpulse;
    m_points[m_pointCount].separation := worldManifold.separations[i];
    Inc(m_pointCount);
    Inc(i);
  end;
end;

procedure TTester.ShiftOrigin(const newOrigin: TVector2);
begin
	m_world.ShiftOrigin(newOrigin);
end;

procedure TTester.UpdateGravityText;
begin
  if FTesterForm <> nil then
    FTesterForm.UpdateGravityText(
      FloatToStr(m_world.Gravity.x), FloatToStr(m_world.Gravity.y));
end;

initialization
   TestCount := 0;
   ActiveEntry := nil;
   _QueryCallback := TQueryCallback.Create;

finalization
   _QueryCallback.Free;

end.
