//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Test;

interface

uses Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

const
  DEFAULT_OPACITY: Double = 100;
  k_maxContactPoints = 2048;

type
  float32 = Single;

type
  PTestCreateFcn = ^TestCreateFcn;
  PSettings = ^Settings;
  PTestEntry = ^TestEntry;
  PContactPoint = ^ContactPoint;
  TTest = class;

  TestCreateFcn = function: TTest;

  /// Test settings. Some can be controlled in the GUI.
  Settings = record
  var
    hz: float32;
    velocityIterations: int32;
    positionIterations: int32;
    drawShapes: Boolean;
    drawJoints: Boolean;
    drawAABBs: Boolean;
    drawContactPoints: Boolean;
    drawContactNormals: Boolean;
    drawContactImpulse: Boolean;
    drawFrictionImpulse: Boolean;
    drawCOMs: Boolean;
    drawStats: Boolean;
    drawProfile: Boolean;
    enableWarmStarting: Boolean;
    enableContinuous: Boolean;
    enableSubStepping: Boolean;
    enableSleep: Boolean;
    pause: Boolean;
    singleStep: Boolean;

    class function Create: Settings; static;
  end;

  TestEntry = record
  var
    name: string;
    createFcn: TestCreateFcn;
    class function Create(const name: string; const CreateFcn: TestCreateFcn): TestEntry; static;
  end;

  /// Joints and fixtures are destroyed when their associated
  /// body is destroyed. Implement this listener so that you
  /// may nullify references to these joints and shapes.
  DestructionListener = class(TNoRefCount, Ib2DestructionListener)
  private
    FHandle: b2DestructionListenerHandle;
    FTest: TTest;
  public
    constructor Create;
    destructor Destroy; override;

    /// Called when any fixture is about to be destroyed due
    /// to the destruction of its parent body.
    procedure SayGoodbye(fixture: Pb2Fixture); overload; cdecl;
    /// Called when any joint is about to be destroyed due
    /// to the destruction of one of its attached bodies.
    procedure SayGoodbye(joint: b2JointHandle); overload; cdecl;

    property Test: TTest read FTest write FTest;
    property Handle: b2DestructionListenerHandle read FHandle;
  end;

  ContactPoint = record
  var
    fixtureA: Pb2Fixture;
    fixtureB: Pb2Fixture;
    normal: b2Vec2;
    position: b2Vec2;
    state: b2PointState;
    normalImpulse: float32;
    tangentImpulse: float32;
    separation: float32;
  end;

  /// Implement this class to get contact information. You can use these results for
  /// things like sounds and game logic. You can also get contact results by
  /// traversing the contact lists after the time step. However, you might miss
  /// some contacts because continuous physics leads to sub-stepping.
  /// Additionally you may receive multiple callbacks for the same contact in a
  /// single time step.
  /// You should strive to make your callbacks efficient because there may be
  /// many callbacks per time step.
  ///
  /// You cannot create/destroy Box2D entities inside these callbacks.
  TTest = class(TNoRefCount, Ib2ContactListener)
  protected
    m_groundBody: b2BodyWrapper;
    m_worldAABB: b2AABB;
    m_points: array [0..k_maxContactPoints-1] of ContactPoint;
    m_pointCount: int32;
    m_destructionListener: DestructionListener;
    m_textLine: int32;
    m_world: b2WorldWrapper;
    m_bomb: b2BodyWrapper;
    m_mouseJoint: b2MouseJointWrapper;
    m_bombSpawnPoint: b2Vec2;
    m_bombSpawning: Boolean;
    m_mouseWorld: b2Vec2;
    m_stepCount: int32;

    m_maxProfile: b2Profile;
    m_totalProfile: b2Profile;
    m_contactListener: b2ContactListenerHandle;

  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawTitle(const str: string);
    procedure Step(settings: PSettings); virtual;
    procedure Keyboard(key: Integer); virtual;
    procedure KeyboardUp(key: Integer); virtual;
    procedure ShiftMouseDown(const [ref] p: b2Vec2);
    procedure MouseDown(const [ref] p: b2Vec2); virtual;
    procedure MouseUp(const [ref] p: b2Vec2); virtual;
    procedure MouseMove(const [ref] p: b2Vec2);
    procedure LaunchBomb; overload;
    procedure LaunchBomb(const [ref] position: b2Vec2; const [ref] velocity: b2Vec2); overload;
    procedure SpawnBomb(const [ref] worldPt: b2Vec2);
    procedure CompleteBombSpawn(const [ref] p: b2Vec2);
    procedure JointDestroyed(joint: b2JointHandle);
    /// Called when two fixtures begin to touch.
    procedure BeginContact(contact: b2ContactHandle); virtual; cdecl;
    /// Called when two fixtures cease to touch.
    procedure EndContact(contact: b2ContactHandle); virtual; cdecl;
    /// This is called after a contact is updated. This allows you to inspect a
    /// contact before it goes to the solver. If you are careful, you can modify the
    /// contact manifold (e.g. disable contact).
    /// A copy of the old manifold is provided so that you can detect changes.
    /// Note: this is called only for awake bodies.
    /// Note: this is called even when the number of contact points is zero.
    /// Note: this is not called for sensors.
    /// Note: if you set the number of contact points to zero, you will not
    /// get an EndContact callback. However, you may get a BeginContact callback
    /// the next step.
    procedure PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold); virtual; cdecl;
    /// This lets you inspect a contact after the solver is finished. This is useful
    /// for inspecting impulses.
    /// Note: the contact manifold does not include time of impact impulses, which can be
    /// arbitrarily large if the sub-step is small. Hence the impulse is provided explicitly
    /// in a separate data structure.
    /// Note: this is only called for contacts that are touching, solid, and awake.
    procedure PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse); virtual; cdecl;
    procedure ShiftOrigin(const [ref] newOrigin: b2Vec2);
  end;


/// Random number in range [-1,1]
function RandomFloat: float32; overload;
/// Random floating point number in range [lo, hi]
function RandomFloat(lo: float32; hi: float32): float32; overload;

procedure RegisterTest(const entry: TestEntry);

var
 g_testEntries: array of TestEntry;

implementation

const
  DRAW_STRING_NEW_LINE = 16;

{ DestructionListener }

constructor DestructionListener.Create;
begin
  FHandle := Create_b2DestructionListener_delegate(Self);
end;

destructor DestructionListener.Destroy;
begin
  Destroy_b2DestructionListener_delegate(FHandle);
  inherited;
end;

procedure DestructionListener.SayGoodbye(fixture: Pb2Fixture);
begin
  // NOP
end;

procedure DestructionListener.SayGoodbye(joint: b2JointHandle);
begin
  if (FTest.m_mouseJoint.FHandle = joint) then
  begin
    FTest.m_mouseJoint := 0;
  end
  else
  begin
    FTest.JointDestroyed(joint);
  end;
end;

{ Settings }

class function Settings.Create: Settings;
begin
  Result.hz := 60.0;
  Result.velocityIterations := 8;
  Result.positionIterations := 3;
  Result.drawShapes := true;
  Result.drawJoints := true;
  Result.drawAABBs := false;
  Result.drawContactPoints := false;
  Result.drawContactNormals := false;
  Result.drawContactImpulse := false;
  Result.drawFrictionImpulse := false;
  Result.drawCOMs := false;
  Result.drawStats := false;
  Result.drawProfile := false;
  Result.enableWarmStarting := true;
  Result.enableContinuous := true;
  Result.enableSubStepping := false;
  Result.enableSleep := true;
  Result.pause := false;
  Result.singleStep := false;
end;

{ TTest }

constructor TTest.Create;
var
  gravity: b2Vec2;
  bodyDef: b2BodyDef;
begin
  gravity := b2Vec2.Create(0.0, -10.0);
  m_world := b2WorldWrapper.Create(gravity);
  m_bomb := 0;
  m_textLine := 30;
  m_mouseJoint := 0;
  m_pointCount := 0;

  m_destructionListener := DestructionListener.Create;
  m_destructionListener.Test := Self;
  m_world.SetDestructionListener(m_destructionListener.Handle);
  m_contactListener := Create_b2ContactListener_delegate(Self);
  m_world.SetContactListener(m_contactListener);
  m_world.SetDebugDraw(g_debugDraw.DrawHandle);

  m_bombSpawning := False;
  m_stepCount := 0;
  bodyDef := b2BodyDef.Create;
  m_groundBody := m_world.CreateBody(@bodyDef);
end;

destructor TTest.Destroy;
begin
  // By deleting the world, we delete the bomb, mouse joint, etc.
  m_world.Destroy;
  m_World := 0;
  m_destructionListener.Free;
  m_destructionListener := nil;
  Destroy_b2ContactListener_delegate(m_contactListener);

  inherited;
end;

procedure TTest.BeginContact(contact: b2ContactHandle);
begin
// EMPTY
end;

procedure TTest.CompleteBombSpawn(const [ref] p: b2Vec2);
const
  multiplier = 30.0;
var
  vel: b2Vec2;
begin
  if m_bombSpawning = False then
    Exit;

  vel := m_bombSpawnPoint - p;
  vel := vel * multiplier;
  LaunchBomb(m_bombSpawnPoint, vel);
  m_bombSpawning := false;
end;

procedure TTest.DrawTitle(const str: string);
begin
  g_debugDraw.DrawString(5, DRAW_STRING_NEW_LINE, str);
  m_textLine := 3 * DRAW_STRING_NEW_LINE;
end;

procedure TTest.EndContact(contact: b2ContactHandle);
begin
// EMPTY
end;

procedure TTest.JointDestroyed(joint: b2JointHandle);
begin
// EMPTY
end;

procedure TTest.Keyboard(key: Integer);
begin
// EMPTY
end;

procedure TTest.KeyboardUp(key: Integer);
begin
// EMPTY
end;

procedure TTest.LaunchBomb;
var
  p, v: b2Vec2;
begin
  p := b2Vec2.Create(RandomFloat(-15.0, 15.0), 30.0);
  v := -5.0 * p;
  LaunchBomb(p, v);
end;

procedure TTest.LaunchBomb(const [ref] position, velocity: b2Vec2);
var
  bd: b2BodyDef;
  circle: b2CircleShapeWrapper;
  fd: b2FixtureDef;
  minV, maxV: b2Vec2;
  aabb: b2AABB;
begin
//  if (m_bomb)
//  {
//    m_world->DestroyBody(m_bomb);
//    m_bomb = NULL;
//  }
//
//  b2BodyDef bd;
//  bd.type = b2_dynamicBody;
//  bd.position = position;
//  bd.bullet = true;
//  m_bomb = m_world->CreateBody(&bd);
//  m_bomb->SetLinearVelocity(velocity);
//
//  b2CircleShape circle;
//  circle.m_radius = 0.3f;
//
//  b2FixtureDef fd;
//  fd.shape = &circle;
//  fd.density = 20.0f;
//  fd.restitution = 0.0f;
//
//  b2Vec2 minV = position - b2Vec2(0.3f,0.3f);
//  b2Vec2 maxV = position + b2Vec2(0.3f,0.3f);
//
//  b2AABB aabb;
//  aabb.lowerBound = minV;
//  aabb.upperBound = maxV;
//
//  m_bomb->CreateFixture(&fd);

  if m_bomb.FHandle <> 0 then
  begin
    m_world.DestroyBody(m_bomb);
    m_bomb.FHandle := 0;
  end;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position := position;
  bd.bullet := true;
  m_bomb := m_world.CreateBody(@bd);
  m_bomb.SetLinearVelocity(velocity);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.3;

  fd := b2FixtureDef.Create;
  fd.shape := circle;
  fd.density := 20.0;
  fd.restitution := 0.0;

  minV := position - b2Vec2.Create(0.3,0.3);
  maxV := position + b2Vec2.Create(0.3,0.3);

  aabb.lowerBound := minV;
  aabb.upperBound := maxV;

  m_bomb.CreateFixture(@fd);
  circle.Destroy;
end;

type
  QueryCallback = class(TNoRefCount, Ib2QueryCallback)
  private
    FHandle: b2QueryCallbackHandle;
  public
    m_point: b2Vec2;
    m_fixture: Pb2Fixture;

    constructor Create(const [ref] point: b2Vec2);
    destructor Destroy; override;
    function ReportFixture(fixture: Pb2Fixture): Boolean;  cdecl;
    property Handle: b2QueryCallbackHandle read FHandle;
  end;

{ QueryCallback }

constructor QueryCallback.Create;
begin
  m_point := point;
  FHandle := Create_b2QueryCallback_delegate(Self);
end;

destructor QueryCallback.Destroy;
begin
  Destroy_b2QueryCallback_delegate(FHandle);
  FHandle := 0;
  inherited;
end;

function QueryCallback.ReportFixture(fixture: Pb2Fixture): Boolean;
var
  body: b2BodyWrapper;
  inside: Boolean;
begin
  body := fixture^.GetBody();
  if body.GetType = b2_dynamicBody then
  begin
    inside := fixture^.TestPoint(m_point);
    if inside then
    begin
      m_fixture := fixture;
      // We are done, terminate the query.
      Exit(False);
    end;
  end;

  // Continue the query.
  Result := True;
end;

procedure TTest.MouseDown(const [ref] p: b2Vec2);
var
  aabb: b2AABB;
  d: b2Vec2;
  callback: QueryCallback;
  body: b2BodyWrapper;
  md: b2MouseJointDef;
begin
  m_mouseWorld := p;

  if m_mouseJoint.FHandle <> 0 then
    Exit;

  // Make a small box.
  d.&Set(0.001, 0.001);
  aabb.lowerBound := p - d;
  aabb.upperBound := p + d;

  // Query the world for overlapping shapes.
  callback := QueryCallback.Create(p);
  m_world.QueryAABB(callback.Handle, aabb);

  if callback.m_fixture <> nil then
  begin
    body := callback.m_fixture.GetBody;
    md := b2MouseJointDef.Create;
    md.bodyA := m_groundBody;
    md.bodyB := body;
    md.target := p;
    md.maxForce := 1000.0 * body.GetMass;
    m_mouseJoint := m_world.CreateJoint(@md);
    body.SetAwake(true);
  end;
  callback.Free;
end;

procedure TTest.MouseMove(const [ref] p: b2Vec2);
begin
  m_mouseWorld := p;

  if m_mouseJoint.FHandle <> 0 then
    m_mouseJoint.SetTarget(p);
end;

procedure TTest.MouseUp(const [ref] p: b2Vec2);
begin
  if (m_mouseJoint.FHandle <> 0) then
  begin
    m_world.DestroyJoint(m_mouseJoint);
    m_mouseJoint.FHandle := 0;
  end;

  if m_bombSpawning then
    CompleteBombSpawn(p);
end;

procedure TTest.PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse);
begin
// EMPTY
end;

procedure TTest.PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold);
const
  b2_maxManifoldPoints = 2;
var
  LContact: b2ContactWrapper;
  manifold: pb2Manifold;
  fixtureA: pb2Fixture;
  fixtureB: pb2Fixture;
  state1: array[0..b2_maxManifoldPoints] of b2PointState;
  state2: array[0..b2_maxManifoldPoints] of b2PointState;
  worldManifold: b2WorldManifold;
  cp: PContactPoint;
  I: Integer;
begin
  LContact := contact;

  manifold := LContact.GetManifold;

  if manifold.pointCount = 0 then
    Exit;

  fixtureA := LContact.GetFixtureA;
  fixtureB := LContact.GetFixtureB;

  b2GetPointStates(@state1[0], @state2[0], oldManifold, manifold);

  LContact.GetWorldManifold(@worldManifold);

  for I := 0 to manifold^.pointCount - 1 do
  begin
    if m_pointCount >= k_maxContactPoints then
      Break;

    cp := @m_points[m_pointCount];
    cp^.fixtureA := fixtureA;
    cp^.fixtureB := fixtureB;
    cp^.position := worldManifold.points[i];
    cp^.normal := worldManifold.normal;
    cp^.state := state2[i];
    cp^.normalImpulse := manifold^.points[i].normalImpulse;
    cp^.tangentImpulse := manifold^.points[i].tangentImpulse;
    cp^.separation := worldManifold.separations[i];
    Inc(m_pointCount);
  end;
end;

procedure TTest.ShiftMouseDown(const [ref] p: b2Vec2);
begin
  m_mouseWorld := p;

  if m_mouseJoint.FHandle <> 0 then
    Exit;

  SpawnBomb(p);
end;

procedure TTest.ShiftOrigin(const [ref] newOrigin: b2Vec2);
begin
  m_world.ShiftOrigin(newOrigin);
end;

procedure TTest.SpawnBomb(const [ref] worldPt: b2Vec2);
begin
  m_bombSpawnPoint := worldPt;
  m_bombSpawning := True;
end;

procedure TTest.Step(settings: PSettings);
var
  timeStep: float32;
  flags: Cardinal;
  bodyCount, contactCount, jointCount: int32;
  proxyCount, height, balance: int32;
  quality: float32;
  p1, p2: b2Vec2;
  c: b2Color;
begin
  if (settings.hz > 0) then
    timeStep := 1.0/settings.hz
  else
    timeStep := 0.0;

  if settings.pause then
  begin
    if settings.singleStep then
    begin
      settings.singleStep := False;
    end
    else
    begin
      timeStep := 0.0;
    end;

    g_debugDraw.DrawString(5, m_textLine, '****PAUSED****');
    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  end;

  flags := 0;
  if settings.drawShapes then
    flags := flags + b2DrawWrapper.e_shapeBit;
  if settings.drawJoints then
    flags := flags + b2DrawWrapper.e_jointBit;
  if settings.drawAABBs then
    flags := flags + b2DrawWrapper.e_aabbBit;
  if settings.drawCOMs then
    flags := flags + b2DrawWrapper.e_centerOfMassBit;

  g_debugDraw.SetFlags(flags);

  m_world.SetAllowSleeping(settings.enableSleep);
  m_world.SetWarmStarting(settings.enableWarmStarting);
  m_world.SetContinuousPhysics(settings.enableContinuous);
  m_world.SetSubStepping(settings.enableSubStepping);

  m_pointCount := 0;

  m_world.Step(timeStep, settings.velocityIterations, settings.positionIterations);

  m_world.DrawDebugData();
  g_debugDraw.Flush();


  if (timeStep > 0.0) then
    Inc(m_stepCount);

  if (settings.drawStats) then
  begin
    bodyCount := m_world.GetBodyCount;
    contactCount := m_world.GetContactCount;
    jointCount := m_world.GetJointCount;
    g_debugDraw.DrawString(5, m_textLine, 'bodies/contacts/joints = %d/%d/%d', [bodyCount, contactCount, jointCount]);
    Inc(m_textLine, DRAW_STRING_NEW_LINE);

    proxyCount := m_world.GetProxyCount;
    height := m_world.GetTreeHeight;
    balance := m_world.GetTreeBalance;
    quality := m_world.GetTreeQuality;
    g_debugDraw.DrawString(5, m_textLine, 'proxies/height/balance/quality = %d/%d/%d/%g', [proxyCount, height, balance, quality]);
    Inc(m_textLine, DRAW_STRING_NEW_LINE);
  end;

//
//  // Track maximum profile times
//  {
//    const b2Profile& p = m_world->GetProfile();
//    m_maxProfile.step = b2Max(m_maxProfile.step, p.step);
//    m_maxProfile.collide = b2Max(m_maxProfile.collide, p.collide);
//    m_maxProfile.solve = b2Max(m_maxProfile.solve, p.solve);
//    m_maxProfile.solveInit = b2Max(m_maxProfile.solveInit, p.solveInit);
//    m_maxProfile.solveVelocity = b2Max(m_maxProfile.solveVelocity, p.solveVelocity);
//    m_maxProfile.solvePosition = b2Max(m_maxProfile.solvePosition, p.solvePosition);
//    m_maxProfile.solveTOI = b2Max(m_maxProfile.solveTOI, p.solveTOI);
//    m_maxProfile.broadphase = b2Max(m_maxProfile.broadphase, p.broadphase);
//
//    m_totalProfile.step += p.step;
//    m_totalProfile.collide += p.collide;
//    m_totalProfile.solve += p.solve;
//    m_totalProfile.solveInit += p.solveInit;
//    m_totalProfile.solveVelocity += p.solveVelocity;
//    m_totalProfile.solvePosition += p.solvePosition;
//    m_totalProfile.solveTOI += p.solveTOI;
//    m_totalProfile.broadphase += p.broadphase;
//  }
//
//  if (settings->drawProfile)
//  {
//    const b2Profile& p = m_world->GetProfile();
//
//    b2Profile aveProfile;
//    memset(&aveProfile, 0, sizeof(b2Profile));
//    if (m_stepCount > 0)
//    {
//      float32 scale = 1.0f / m_stepCount;
//      aveProfile.step = scale * m_totalProfile.step;
//      aveProfile.collide = scale * m_totalProfile.collide;
//      aveProfile.solve = scale * m_totalProfile.solve;
//      aveProfile.solveInit = scale * m_totalProfile.solveInit;
//      aveProfile.solveVelocity = scale * m_totalProfile.solveVelocity;
//      aveProfile.solvePosition = scale * m_totalProfile.solvePosition;
//      aveProfile.solveTOI = scale * m_totalProfile.solveTOI;
//      aveProfile.broadphase = scale * m_totalProfile.broadphase;
//    }
//
//    g_debugDraw.DrawString(5, m_textLine, "step [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.step, aveProfile.step, m_maxProfile.step);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "collide [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.collide, aveProfile.collide, m_maxProfile.collide);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "solve [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.solve, aveProfile.solve, m_maxProfile.solve);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "solve init [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.solveInit, aveProfile.solveInit, m_maxProfile.solveInit);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "solve velocity [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.solveVelocity, aveProfile.solveVelocity, m_maxProfile.solveVelocity);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "solve position [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.solvePosition, aveProfile.solvePosition, m_maxProfile.solvePosition);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "solveTOI [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.solveTOI, aveProfile.solveTOI, m_maxProfile.solveTOI);
//    m_textLine += DRAW_STRING_NEW_LINE;
//    g_debugDraw.DrawString(5, m_textLine, "broad-phase [ave] (max) = %5.2f [%6.2f] (%6.2f)", p.broadphase, aveProfile.broadphase, m_maxProfile.broadphase);
//    m_textLine += DRAW_STRING_NEW_LINE;
//  }
//
//  if (m_mouseJoint)
//  {
//    b2Vec2 p1 = m_mouseJoint->GetAnchorB();
//    b2Vec2 p2 = m_mouseJoint->GetTarget();
//
//    b2Color c;
//    c.Set(0.0f, 1.0f, 0.0f);
//    g_debugDraw.DrawPoint(p1, 4.0f, c);
//    g_debugDraw.DrawPoint(p2, 4.0f, c);
//
//    c.Set(0.8f, 0.8f, 0.8f);
//    g_debugDraw.DrawSegment(p1, p2, c);
//  }

  if (m_mouseJoint.FHandle <> 0) then
  begin
    p1 := m_mouseJoint.GetAnchorA;
    p2 := m_mouseJoint.GetAnchorB;
    c.&Set(0.0, 1.0, 0.0, 1.0);
    g_debugDraw.DrawPoint(p1, 4.0, c);
    g_debugDraw.DrawPoint(p2, 4.0, c);

    c.&Set(0.8, 0.8, 0.8, 1.0);
    g_debugDraw.DrawSegment(p1, p2, c);
  end;

//
//  if (m_bombSpawning)
//  {
//    b2Color c;
//    c.Set(0.0f, 0.0f, 1.0f);
//    g_debugDraw.DrawPoint(m_bombSpawnPoint, 4.0f, c);
//
//    c.Set(0.8f, 0.8f, 0.8f);
//    g_debugDraw.DrawSegment(m_mouseWorld, m_bombSpawnPoint, c);
//  }

  if (m_bombSpawning) then
  begin
    c.&Set(0.0, 0.0, 1.0, 1.0);
    g_debugDraw.DrawPoint(m_bombSpawnPoint, 4.0, c);

    c.&Set(0.8, 0.8, 0.8, 1.0);
    g_debugDraw.DrawSegment(m_mouseWorld, m_bombSpawnPoint, c);
  end;

//
//  if (settings->drawContactPoints)
//  {
//    const float32 k_impulseScale = 0.1f;
//    const float32 k_axisScale = 0.3f;
//
//    for (int32 i = 0; i < m_pointCount; ++i)
//    {
//      ContactPoint* point = m_points + i;
//
//      if (point->state == b2_addState)
//      {
//        // Add
//        g_debugDraw.DrawPoint(point->position, 10.0f, b2Color(0.3f, 0.95f, 0.3f));
//      }
//      else if (point->state == b2_persistState)
//      {
//        // Persist
//        g_debugDraw.DrawPoint(point->position, 5.0f, b2Color(0.3f, 0.3f, 0.95f));
//      }
//
//      if (settings->drawContactNormals == 1)
//      {
//        b2Vec2 p1 = point->position;
//        b2Vec2 p2 = p1 + k_axisScale * point->normal;
//        g_debugDraw.DrawSegment(p1, p2, b2Color(0.9f, 0.9f, 0.9f));
//      }
//      else if (settings->drawContactImpulse == 1)
//      {
//        b2Vec2 p1 = point->position;
//        b2Vec2 p2 = p1 + k_impulseScale * point->normalImpulse * point->normal;
//        g_debugDraw.DrawSegment(p1, p2, b2Color(0.9f, 0.9f, 0.3f));
//      }
//
//      if (settings->drawFrictionImpulse == 1)
//      {
//        b2Vec2 tangent = b2Cross(point->normal, 1.0f);
//        b2Vec2 p1 = point->position;
//        b2Vec2 p2 = p1 + k_impulseScale * point->tangentImpulse * tangent;
//        g_debugDraw.DrawSegment(p1, p2, b2Color(0.9f, 0.9f, 0.3f));
//      }
//    }
//  }
end;


{ TestEntry }

class function TestEntry.Create(const name: string;
                                const CreateFcn: TestCreateFcn): TestEntry;
begin
  Result.name := name;
  Result.createFcn := CreateFcn;
end;

/// Random number in range [-1,1]
function RandomFloat: float32;
begin
  Result := Random();
  Result := 2.0 * Result - 1.0;
end;

/// Random floating point number in range [lo, hi]
function RandomFloat(lo: float32; hi: float32): float32;
begin
  Result := Random();
  Result := (hi-lo) * Result + lo;
end;

procedure RegisterTest(const entry: TestEntry);
var
  L: Integer;
begin
  L := Length(g_testEntries);
  SetLength(g_testEntries, L+1);
  g_testEntries[L] := entry;
end;

end.


