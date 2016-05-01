
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Box2D.Dynamics;

interface

uses
Box2D.Collision,
Box2D.Common,
Box2DTypes;

const
b2_minPulleyLength: Single = 2.000000;


type

{$MinEnumSize 4}
b2BodyType = (b2_staticBody = 0, b2_kinematicBody = 1, b2_dynamicBody = 2);
{$MinEnumSize 1}
Pb2BodyType = ^b2BodyType;

{$MinEnumSize 4}
b2JointType = (e_unknownJoint = 0, e_revoluteJoint = 1, e_prismaticJoint = 2, e_distanceJoint = 3, e_pulleyJoint = 4, e_mouseJoint = 5, e_gearJoint = 6, e_wheelJoint = 7, e_weldJoint = 8, e_frictionJoint = 9, e_ropeJoint = 10, e_motorJoint = 11);
{$MinEnumSize 1}
Pb2JointType = ^b2JointType;

{$MinEnumSize 4}
b2LimitState = (e_inactiveLimit = 0, e_atLowerLimit = 1, e_atUpperLimit = 2, e_equalLimits = 3);
{$MinEnumSize 1}
Pb2LimitState = ^b2LimitState;


b2JointHandle = THandle;
Pb2JointHandle = ^b2JointHandle;

b2ContactHandle = THandle;
Pb2ContactHandle = ^b2ContactHandle;

b2WorldHandle = THandle;
Pb2WorldHandle = ^b2WorldHandle;

b2BodyHandle = THandle;
Pb2BodyHandle = ^b2BodyHandle;

b2ContactFilterHandle = THandle;
Pb2ContactFilterHandle = ^b2ContactFilterHandle;

b2ContactListenerHandle = THandle;
Pb2ContactListenerHandle = ^b2ContactListenerHandle;

b2ContactManagerHandle = THandle;
Pb2ContactManagerHandle = ^b2ContactManagerHandle;

b2IslandHandle = THandle;
Pb2IslandHandle = ^b2IslandHandle;

b2DestructionListenerHandle = THandle;
Pb2DestructionListenerHandle = ^b2DestructionListenerHandle;

b2QueryCallbackHandle = THandle;
Pb2QueryCallbackHandle = ^b2QueryCallbackHandle;

b2RayCastCallbackHandle = THandle;
Pb2RayCastCallbackHandle = ^b2RayCastCallbackHandle;

b2ChainAndCircleContactHandle = THandle;
Pb2ChainAndCircleContactHandle = ^b2ChainAndCircleContactHandle;

b2ChainAndPolygonContactHandle = THandle;
Pb2ChainAndPolygonContactHandle = ^b2ChainAndPolygonContactHandle;

b2CircleContactHandle = THandle;
Pb2CircleContactHandle = ^b2CircleContactHandle;

b2ContactSolverHandle = THandle;
Pb2ContactSolverHandle = ^b2ContactSolverHandle;

b2EdgeAndCircleContactHandle = THandle;
Pb2EdgeAndCircleContactHandle = ^b2EdgeAndCircleContactHandle;

b2EdgeAndPolygonContactHandle = THandle;
Pb2EdgeAndPolygonContactHandle = ^b2EdgeAndPolygonContactHandle;

b2PolygonAndCircleContactHandle = THandle;
Pb2PolygonAndCircleContactHandle = ^b2PolygonAndCircleContactHandle;

b2PolygonContactHandle = THandle;
Pb2PolygonContactHandle = ^b2PolygonContactHandle;

b2DistanceJointHandle = THandle;
Pb2DistanceJointHandle = ^b2DistanceJointHandle;

b2FrictionJointHandle = THandle;
Pb2FrictionJointHandle = ^b2FrictionJointHandle;

b2GearJointHandle = THandle;
Pb2GearJointHandle = ^b2GearJointHandle;

b2MotorJointHandle = THandle;
Pb2MotorJointHandle = ^b2MotorJointHandle;

b2MouseJointHandle = THandle;
Pb2MouseJointHandle = ^b2MouseJointHandle;

b2PrismaticJointHandle = THandle;
Pb2PrismaticJointHandle = ^b2PrismaticJointHandle;

b2PulleyJointHandle = THandle;
Pb2PulleyJointHandle = ^b2PulleyJointHandle;

b2RevoluteJointHandle = THandle;
Pb2RevoluteJointHandle = ^b2RevoluteJointHandle;

b2RopeJointHandle = THandle;
Pb2RopeJointHandle = ^b2RopeJointHandle;

b2WeldJointHandle = THandle;
Pb2WeldJointHandle = ^b2WeldJointHandle;

b2WheelJointHandle = THandle;
Pb2WheelJointHandle = ^b2WheelJointHandle;


Pb2Fixture = ^b2Fixture;
PPb2Fixture = ^Pb2Fixture;

Pb2FixtureDef = ^b2FixtureDef;
PPb2FixtureDef = ^Pb2FixtureDef;

Pb2JointEdge = ^b2JointEdge;
PPb2JointEdge = ^Pb2JointEdge;

Pb2ContactEdge = ^b2ContactEdge;
PPb2ContactEdge = ^Pb2ContactEdge;

Pb2BodyDef = ^b2BodyDef;
PPb2BodyDef = ^Pb2BodyDef;

Pb2Filter = ^b2Filter;
PPb2Filter = ^Pb2Filter;

Pb2FixtureProxy = ^b2FixtureProxy;
PPb2FixtureProxy = ^Pb2FixtureProxy;

Pb2Profile = ^b2Profile;
PPb2Profile = ^Pb2Profile;

Pb2TimeStep = ^b2TimeStep;
PPb2TimeStep = ^Pb2TimeStep;

Pb2Position = ^b2Position;
PPb2Position = ^Pb2Position;

Pb2Velocity = ^b2Velocity;
PPb2Velocity = ^Pb2Velocity;

Pb2SolverData = ^b2SolverData;
PPb2SolverData = ^Pb2SolverData;

Pb2ContactVelocityConstraint = ^b2ContactVelocityConstraint;
PPb2ContactVelocityConstraint = ^Pb2ContactVelocityConstraint;

Pb2ContactImpulse = ^b2ContactImpulse;
PPb2ContactImpulse = ^Pb2ContactImpulse;

Pb2JointDef = ^b2JointDef;
PPb2JointDef = ^Pb2JointDef;

b2ContactCreateFcn = function(param1: Pb2Fixture; param2: Integer; param3: Pb2Fixture; param4: Integer; param5: b2BlockAllocatorHandle): b2ContactHandle; stdcall;
b2ContactDestroyFcn = procedure(param1: b2ContactHandle; param2: b2BlockAllocatorHandle); stdcall;
Pb2ContactRegister = ^b2ContactRegister;
PPb2ContactRegister = ^Pb2ContactRegister;

Pb2VelocityConstraintPoint = ^b2VelocityConstraintPoint;
PPb2VelocityConstraintPoint = ^Pb2VelocityConstraintPoint;

Pb2ContactSolverDef = ^b2ContactSolverDef;
PPb2ContactSolverDef = ^Pb2ContactSolverDef;

Pb2Jacobian = ^b2Jacobian;
PPb2Jacobian = ^Pb2Jacobian;

Pb2DistanceJointDef = ^b2DistanceJointDef;
PPb2DistanceJointDef = ^Pb2DistanceJointDef;

Pb2FrictionJointDef = ^b2FrictionJointDef;
PPb2FrictionJointDef = ^Pb2FrictionJointDef;

Pb2GearJointDef = ^b2GearJointDef;
PPb2GearJointDef = ^Pb2GearJointDef;

Pb2MotorJointDef = ^b2MotorJointDef;
PPb2MotorJointDef = ^Pb2MotorJointDef;

Pb2MouseJointDef = ^b2MouseJointDef;
PPb2MouseJointDef = ^Pb2MouseJointDef;

Pb2PrismaticJointDef = ^b2PrismaticJointDef;
PPb2PrismaticJointDef = ^Pb2PrismaticJointDef;

Pb2PulleyJointDef = ^b2PulleyJointDef;
PPb2PulleyJointDef = ^Pb2PulleyJointDef;

Pb2RevoluteJointDef = ^b2RevoluteJointDef;
PPb2RevoluteJointDef = ^Pb2RevoluteJointDef;

Pb2RopeJointDef = ^b2RopeJointDef;
PPb2RopeJointDef = ^Pb2RopeJointDef;

Pb2WeldJointDef = ^b2WeldJointDef;
PPb2WeldJointDef = ^Pb2WeldJointDef;

Pb2WheelJointDef = ^b2WheelJointDef;
PPb2WheelJointDef = ^Pb2WheelJointDef;


{ ===== Records ===== }

b2BodyDef = record
&type: b2BodyType;
position: b2Vec2;
angle: Single;
linearVelocity: b2Vec2;
angularVelocity: Single;
linearDamping: Single;
angularDamping: Single;
allowSleep: Boolean;
awake: Boolean;
fixedRotation: Boolean;
bullet: Boolean;
active: Boolean;
userData: Pointer;
gravityScale: Single;

class function Create: b2BodyDef; static; cdecl;
end;

b2BodyWrapper = record
FHandle: b2BodyHandle;

class operator Implicit(handle: b2BodyHandle): b2BodyWrapper; overload;
class operator Implicit(wrapper: b2BodyWrapper): b2BodyHandle; overload;
function CreateFixture(def: Pb2FixtureDef): Pb2Fixture; overload; cdecl;
function CreateFixture(shape: b2ShapeHandle; density: Single): Pb2Fixture; overload; cdecl;
procedure DestroyFixture(fixture: Pb2Fixture); cdecl;
procedure SetTransform(const [ref] position: b2Vec2; angle: Single); cdecl;
function GetTransform: Pb2Transform; cdecl;
function GetPosition: Pb2Vec2; cdecl;
function GetAngle: Single; cdecl;
function GetWorldCenter: Pb2Vec2; cdecl;
function GetLocalCenter: Pb2Vec2; cdecl;
procedure SetLinearVelocity(const [ref] v: b2Vec2); cdecl;
function GetLinearVelocity: Pb2Vec2; cdecl;
procedure SetAngularVelocity(omega: Single); cdecl;
function GetAngularVelocity: Single; cdecl;
procedure ApplyForce(const [ref] force: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl;
procedure ApplyForceToCenter(const [ref] force: b2Vec2; wake: Boolean); cdecl;
procedure ApplyTorque(torque: Single; wake: Boolean); cdecl;
procedure ApplyLinearImpulse(const [ref] impulse: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl;
procedure ApplyAngularImpulse(impulse: Single; wake: Boolean); cdecl;
function GetMass: Single; cdecl;
function GetInertia: Single; cdecl;
procedure GetMassData(data: Pb2MassData); cdecl;
procedure SetMassData(data: Pb2MassData); cdecl;
procedure ResetMassData; cdecl;
function GetWorldPoint(const [ref] localPoint: b2Vec2): b2Vec2; cdecl;
function GetWorldVector(const [ref] localVector: b2Vec2): b2Vec2; cdecl;
function GetLocalPoint(const [ref] worldPoint: b2Vec2): b2Vec2; cdecl;
function GetLocalVector(const [ref] worldVector: b2Vec2): b2Vec2; cdecl;
function GetLinearVelocityFromWorldPoint(const [ref] worldPoint: b2Vec2): b2Vec2; cdecl;
function GetLinearVelocityFromLocalPoint(const [ref] localPoint: b2Vec2): b2Vec2; cdecl;
function GetLinearDamping: Single; cdecl;
procedure SetLinearDamping(linearDamping: Single); cdecl;
function GetAngularDamping: Single; cdecl;
procedure SetAngularDamping(angularDamping: Single); cdecl;
function GetGravityScale: Single; cdecl;
procedure SetGravityScale(scale: Single); cdecl;
procedure SetType(_type: b2BodyType); cdecl;
function GetType: b2BodyType; cdecl;
procedure SetBullet(flag: Boolean); cdecl;
function IsBullet: Boolean; cdecl;
procedure SetSleepingAllowed(flag: Boolean); cdecl;
function IsSleepingAllowed: Boolean; cdecl;
procedure SetAwake(flag: Boolean); cdecl;
function IsAwake: Boolean; cdecl;
procedure SetActive(flag: Boolean); cdecl;
function IsActive: Boolean; cdecl;
procedure SetFixedRotation(flag: Boolean); cdecl;
function IsFixedRotation: Boolean; cdecl;
function GetFixtureList: Pb2Fixture; cdecl;
function GetJointList: Pb2JointEdge; cdecl;
function GetContactList: Pb2ContactEdge; cdecl;
function GetNext: b2BodyHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function GetWorld: b2WorldHandle; cdecl;
procedure Dump; cdecl;
end;

b2ContactManagerWrapper = record
FHandle: b2ContactManagerHandle;

class function Create: b2ContactManagerWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ContactManagerHandle): b2ContactManagerWrapper; overload;
class operator Implicit(wrapper: b2ContactManagerWrapper): b2ContactManagerHandle; overload;
procedure AddPair(proxyUserDataA: Pointer; proxyUserDataB: Pointer); cdecl;
procedure FindNewContacts; cdecl;
procedure Destroy_(c: b2ContactHandle); cdecl;
procedure Collide; cdecl;
function Get_m_broadPhase: b2BroadPhaseHandle; cdecl;
procedure Set_m_broadPhase(aNewValue: b2BroadPhaseHandle); cdecl;
function Get_m_contactList: b2ContactHandle; cdecl;
procedure Set_m_contactList(aNewValue: b2ContactHandle); cdecl;
function Get_m_contactCount: Integer; cdecl;
procedure Set_m_contactCount(aNewValue: Integer); cdecl;
function Get_m_contactFilter: b2ContactFilterHandle; cdecl;
procedure Set_m_contactFilter(aNewValue: b2ContactFilterHandle); cdecl;
function Get_m_contactListener: b2ContactListenerHandle; cdecl;
procedure Set_m_contactListener(aNewValue: b2ContactListenerHandle); cdecl;
function Get_m_allocator: b2BlockAllocatorHandle; cdecl;
procedure Set_m_allocator(aNewValue: b2BlockAllocatorHandle); cdecl;
end;

b2Filter = record
categoryBits: Word;
maskBits: Word;
groupIndex: SmallInt;

class function Create: b2Filter; static; cdecl;
end;

b2FixtureDef = record
shape: b2ShapeHandle;
userData: Pointer;
friction: Single;
restitution: Single;
density: Single;
isSensor: Boolean;
filter: b2Filter;

class function Create: b2FixtureDef; static; cdecl;
end;

b2FixtureProxy = record
aabb: b2AABB;
fixture: Pb2Fixture;
childIndex: Integer;
proxyId: Integer;

class function Create: b2FixtureProxy; static; cdecl;
end;

b2Fixture = record
private 
m_density: Single;
m_next: Pb2Fixture;
m_body: b2BodyHandle;
m_shape: b2ShapeHandle;
m_friction: Single;
m_restitution: Single;
m_proxies: Pb2FixtureProxy;
m_proxyCount: Integer;
m_filter: b2Filter;
m_isSensor: Boolean;
m_userData: Pointer;

public 
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetShape: b2ShapeHandle; cdecl;
procedure SetSensor(sensor: Boolean); cdecl;
function IsSensor: Boolean; cdecl;
procedure SetFilterData(const [ref] filter: b2Filter); cdecl;
function GetFilterData: Pb2Filter; cdecl;
procedure Refilter; cdecl;
function GetBody: b2BodyHandle; cdecl;
function GetNext: Pb2Fixture; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function TestPoint(const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; childIndex: Integer): Boolean; cdecl;
procedure GetMassData(massData: Pb2MassData); cdecl;
procedure SetDensity(density: Single); cdecl;
function GetDensity: Single; cdecl;
function GetFriction: Single; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetAABB(childIndex: Integer): Pb2AABB; cdecl;
procedure Dump(bodyIndex: Integer); cdecl;
end;

b2Profile = record
step: Single;
collide: Single;
solve: Single;
solveInit: Single;
solveVelocity: Single;
solvePosition: Single;
broadphase: Single;
solveTOI: Single;

class function Create: b2Profile; static; cdecl;
end;

b2TimeStep = record
dt: Single;
inv_dt: Single;
dtRatio: Single;
velocityIterations: Integer;
positionIterations: Integer;
warmStarting: Boolean;

class function Create: b2TimeStep; static; cdecl;
end;

b2Position = record
c: b2Vec2;
a: Single;

class function Create: b2Position; static; cdecl;
end;

b2Velocity = record
v: b2Vec2;
w: Single;

class function Create: b2Velocity; static; cdecl;
end;

b2SolverData = record
step: b2TimeStep;
positions: Pb2Position;
velocities: Pb2Velocity;

class function Create: b2SolverData; static; cdecl;
end;

b2IslandWrapper = record
FHandle: b2IslandHandle;

class function Create(bodyCapacity: Integer; contactCapacity: Integer; jointCapacity: Integer; allocator: b2StackAllocatorHandle; listener: b2ContactListenerHandle): b2IslandWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2IslandHandle): b2IslandWrapper; overload;
class operator Implicit(wrapper: b2IslandWrapper): b2IslandHandle; overload;
procedure Clear; cdecl;
procedure Solve(profile: Pb2Profile; const [ref] step: b2TimeStep; const [ref] gravity: b2Vec2; allowSleep: Boolean); cdecl;
procedure SolveTOI(const [ref] subStep: b2TimeStep; toiIndexA: Integer; toiIndexB: Integer); cdecl;
procedure Add(body: b2BodyHandle); cdecl;
procedure Add2(contact: b2ContactHandle); cdecl;
procedure Add3(joint: b2JointHandle); cdecl;
procedure Report(constraints: Pb2ContactVelocityConstraint); cdecl;
function Get_m_allocator: b2StackAllocatorHandle; cdecl;
procedure Set_m_allocator(aNewValue: b2StackAllocatorHandle); cdecl;
function Get_m_listener: b2ContactListenerHandle; cdecl;
procedure Set_m_listener(aNewValue: b2ContactListenerHandle); cdecl;
function Get_m_bodies: Pb2BodyHandle; cdecl;
procedure Set_m_bodies(aNewValue: Pb2BodyHandle); cdecl;
function Get_m_contacts: Pb2ContactHandle; cdecl;
procedure Set_m_contacts(aNewValue: Pb2ContactHandle); cdecl;
function Get_m_joints: Pb2JointHandle; cdecl;
procedure Set_m_joints(aNewValue: Pb2JointHandle); cdecl;
function Get_m_positions: Pb2Position; cdecl;
procedure Set_m_positions(aNewValue: Pb2Position); cdecl;
function Get_m_velocities: Pb2Velocity; cdecl;
procedure Set_m_velocities(aNewValue: Pb2Velocity); cdecl;
function Get_m_bodyCount: Integer; cdecl;
procedure Set_m_bodyCount(aNewValue: Integer); cdecl;
function Get_m_jointCount: Integer; cdecl;
procedure Set_m_jointCount(aNewValue: Integer); cdecl;
function Get_m_contactCount: Integer; cdecl;
procedure Set_m_contactCount(aNewValue: Integer); cdecl;
function Get_m_bodyCapacity: Integer; cdecl;
procedure Set_m_bodyCapacity(aNewValue: Integer); cdecl;
function Get_m_contactCapacity: Integer; cdecl;
procedure Set_m_contactCapacity(aNewValue: Integer); cdecl;
function Get_m_jointCapacity: Integer; cdecl;
procedure Set_m_jointCapacity(aNewValue: Integer); cdecl;
end;

b2DestructionListenerWrapper = record
FHandle: b2DestructionListenerHandle;

class operator Implicit(handle: b2DestructionListenerHandle): b2DestructionListenerWrapper; overload;
class operator Implicit(wrapper: b2DestructionListenerWrapper): b2DestructionListenerHandle; overload;
procedure SayGoodbye(joint: b2JointHandle); overload; cdecl;
procedure SayGoodbye(fixture: Pb2Fixture); overload; cdecl;
end;

b2ContactFilterWrapper = record
FHandle: b2ContactFilterHandle;

class function Create: b2ContactFilterWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ContactFilterHandle): b2ContactFilterWrapper; overload;
class operator Implicit(wrapper: b2ContactFilterWrapper): b2ContactFilterHandle; overload;
function ShouldCollide(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): Boolean; cdecl;
end;

b2ContactImpulse = record
normalImpulses: array[0..1] of Single;
tangentImpulses: array[0..1] of Single;
count: Integer;

class function Create: b2ContactImpulse; static; cdecl;
end;

b2ContactListenerWrapper = record
FHandle: b2ContactListenerHandle;

class function Create: b2ContactListenerWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ContactListenerHandle): b2ContactListenerWrapper; overload;
class operator Implicit(wrapper: b2ContactListenerWrapper): b2ContactListenerHandle; overload;
procedure BeginContact(contact: b2ContactHandle); cdecl;
procedure EndContact(contact: b2ContactHandle); cdecl;
procedure PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold); cdecl;
procedure PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse); cdecl;
end;

b2QueryCallbackWrapper = record
FHandle: b2QueryCallbackHandle;

class operator Implicit(handle: b2QueryCallbackHandle): b2QueryCallbackWrapper; overload;
class operator Implicit(wrapper: b2QueryCallbackWrapper): b2QueryCallbackHandle; overload;
function ReportFixture(fixture: Pb2Fixture): Boolean; cdecl;
end;

b2RayCastCallbackWrapper = record
FHandle: b2RayCastCallbackHandle;

class operator Implicit(handle: b2RayCastCallbackHandle): b2RayCastCallbackWrapper; overload;
class operator Implicit(wrapper: b2RayCastCallbackWrapper): b2RayCastCallbackHandle; overload;
function ReportFixture(fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single; cdecl;
end;

b2WorldWrapper = record
FHandle: b2WorldHandle;

class function Create(const [ref] gravity: b2Vec2): b2WorldWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2WorldHandle): b2WorldWrapper; overload;
class operator Implicit(wrapper: b2WorldWrapper): b2WorldHandle; overload;
procedure SetDestructionListener(listener: b2DestructionListenerHandle); cdecl;
procedure SetContactFilter(filter: b2ContactFilterHandle); cdecl;
procedure SetContactListener(listener: b2ContactListenerHandle); cdecl;
procedure SetDebugDraw(debugDraw: b2DrawHandle); cdecl;
function CreateBody(def: Pb2BodyDef): b2BodyHandle; cdecl;
procedure DestroyBody(body: b2BodyHandle); cdecl;
function CreateJoint(def: Pb2JointDef): b2JointHandle; cdecl;
procedure DestroyJoint(joint: b2JointHandle); cdecl;
procedure Step(timeStep: Single; velocityIterations: Integer; positionIterations: Integer); cdecl;
procedure ClearForces; cdecl;
procedure DrawDebugData; cdecl;
procedure QueryAABB(callback: b2QueryCallbackHandle; const [ref] aabb: b2AABB); cdecl;
procedure RayCast(callback: b2RayCastCallbackHandle; const [ref] point1: b2Vec2; const [ref] point2: b2Vec2); cdecl;
function GetBodyList: b2BodyHandle; cdecl;
function GetJointList: b2JointHandle; cdecl;
function GetContactList: b2ContactHandle; cdecl;
procedure SetAllowSleeping(flag: Boolean); cdecl;
function GetAllowSleeping: Boolean; cdecl;
procedure SetWarmStarting(flag: Boolean); cdecl;
function GetWarmStarting: Boolean; cdecl;
procedure SetContinuousPhysics(flag: Boolean); cdecl;
function GetContinuousPhysics: Boolean; cdecl;
procedure SetSubStepping(flag: Boolean); cdecl;
function GetSubStepping: Boolean; cdecl;
function GetProxyCount: Integer; cdecl;
function GetBodyCount: Integer; cdecl;
function GetJointCount: Integer; cdecl;
function GetContactCount: Integer; cdecl;
function GetTreeHeight: Integer; cdecl;
function GetTreeBalance: Integer; cdecl;
function GetTreeQuality: Single; cdecl;
procedure SetGravity(const [ref] gravity: b2Vec2); cdecl;
function GetGravity: b2Vec2; cdecl;
function IsLocked: Boolean; cdecl;
procedure SetAutoClearForces(flag: Boolean); cdecl;
function GetAutoClearForces: Boolean; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetContactManager: b2ContactManagerHandle; cdecl;
function GetProfile: Pb2Profile; cdecl;
procedure Dump; cdecl;
end;

b2ContactRegister = record
createFcn: b2ContactCreateFcn;
destroyFcn: b2ContactDestroyFcn;
primary: Boolean;

class function Create: b2ContactRegister; static; cdecl;
end;

b2ContactEdge = record
other: b2BodyHandle;
contact: b2ContactHandle;
prev: Pb2ContactEdge;
next: Pb2ContactEdge;

class function Create: b2ContactEdge; static; cdecl;
end;

b2ContactWrapper = record
FHandle: b2ContactHandle;

class operator Implicit(handle: b2ContactHandle): b2ContactWrapper; overload;
class operator Implicit(wrapper: b2ContactWrapper): b2ContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
end;

b2ChainAndCircleContactWrapper = record
FHandle: b2ChainAndCircleContactHandle;

class function Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndCircleContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ChainAndCircleContactHandle): b2ChainAndCircleContactWrapper; overload;
class operator Implicit(wrapper: b2ChainAndCircleContactWrapper): b2ChainAndCircleContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2ChainAndPolygonContactWrapper = record
FHandle: b2ChainAndPolygonContactHandle;

class function Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndPolygonContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ChainAndPolygonContactHandle): b2ChainAndPolygonContactWrapper; overload;
class operator Implicit(wrapper: b2ChainAndPolygonContactWrapper): b2ChainAndPolygonContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2CircleContactWrapper = record
FHandle: b2CircleContactHandle;

class function Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2CircleContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2CircleContactHandle): b2CircleContactWrapper; overload;
class operator Implicit(wrapper: b2CircleContactWrapper): b2CircleContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2VelocityConstraintPoint = record
rA: b2Vec2;
rB: b2Vec2;
normalImpulse: Single;
tangentImpulse: Single;
normalMass: Single;
tangentMass: Single;
velocityBias: Single;

class function Create: b2VelocityConstraintPoint; static; cdecl;
end;

b2ContactVelocityConstraint = record
points: array[0..1] of b2VelocityConstraintPoint;
normal: b2Vec2;
normalMass: b2Mat22;
K: b2Mat22;
indexA: Integer;
indexB: Integer;
invMassA: Single;
invMassB: Single;
invIA: Single;
invIB: Single;
friction: Single;
restitution: Single;
tangentSpeed: Single;
pointCount: Integer;
contactIndex: Integer;

class function Create: b2ContactVelocityConstraint; static; cdecl;
end;

b2ContactSolverDef = record
step: b2TimeStep;
contacts: b2ContactHandle;
count: Integer;
positions: Pb2Position;
velocities: Pb2Velocity;
allocator: b2StackAllocatorHandle;

class function Create: b2ContactSolverDef; static; cdecl;
end;

b2ContactSolverWrapper = record
FHandle: b2ContactSolverHandle;

class function Create(def: Pb2ContactSolverDef): b2ContactSolverWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ContactSolverHandle): b2ContactSolverWrapper; overload;
class operator Implicit(wrapper: b2ContactSolverWrapper): b2ContactSolverHandle; overload;
procedure InitializeVelocityConstraints; cdecl;
procedure WarmStart; cdecl;
procedure SolveVelocityConstraints; cdecl;
procedure StoreImpulses; cdecl;
function SolvePositionConstraints: Boolean; cdecl;
function SolveTOIPositionConstraints(toiIndexA: Integer; toiIndexB: Integer): Boolean; cdecl;
function Get_m_step: b2TimeStep; cdecl;
procedure Set_m_step(aNewValue: b2TimeStep); cdecl;
function Get_m_positions: Pb2Position; cdecl;
procedure Set_m_positions(aNewValue: Pb2Position); cdecl;
function Get_m_velocities: Pb2Velocity; cdecl;
procedure Set_m_velocities(aNewValue: Pb2Velocity); cdecl;
function Get_m_allocator: b2StackAllocatorHandle; cdecl;
procedure Set_m_allocator(aNewValue: b2StackAllocatorHandle); cdecl;
function Get_m_velocityConstraints: Pb2ContactVelocityConstraint; cdecl;
procedure Set_m_velocityConstraints(aNewValue: Pb2ContactVelocityConstraint); cdecl;
function Get_m_contacts: Pb2ContactHandle; cdecl;
procedure Set_m_contacts(aNewValue: Pb2ContactHandle); cdecl;
function Get_m_count: Integer; cdecl;
procedure Set_m_count(aNewValue: Integer); cdecl;
end;

b2EdgeAndCircleContactWrapper = record
FHandle: b2EdgeAndCircleContactHandle;

class function Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndCircleContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2EdgeAndCircleContactHandle): b2EdgeAndCircleContactWrapper; overload;
class operator Implicit(wrapper: b2EdgeAndCircleContactWrapper): b2EdgeAndCircleContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2EdgeAndPolygonContactWrapper = record
FHandle: b2EdgeAndPolygonContactHandle;

class function Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndPolygonContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2EdgeAndPolygonContactHandle): b2EdgeAndPolygonContactWrapper; overload;
class operator Implicit(wrapper: b2EdgeAndPolygonContactWrapper): b2EdgeAndPolygonContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2PolygonAndCircleContactWrapper = record
FHandle: b2PolygonAndCircleContactHandle;

class function Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonAndCircleContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2PolygonAndCircleContactHandle): b2PolygonAndCircleContactWrapper; overload;
class operator Implicit(wrapper: b2PolygonAndCircleContactWrapper): b2PolygonAndCircleContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2PolygonContactWrapper = record
FHandle: b2PolygonContactHandle;

class function Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonContactWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2PolygonContactHandle): b2PolygonContactWrapper; overload;
class operator Implicit(wrapper: b2PolygonContactWrapper): b2PolygonContactHandle; overload;
function GetManifold: Pb2Manifold; cdecl;
procedure GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
function IsTouching: Boolean; cdecl;
procedure SetEnabled(flag: Boolean); cdecl;
function IsEnabled: Boolean; cdecl;
function GetNext: b2ContactHandle; cdecl;
function GetFixtureA: Pb2Fixture; cdecl;
function GetChildIndexA: Integer; cdecl;
function GetFixtureB: Pb2Fixture; cdecl;
function GetChildIndexB: Integer; cdecl;
procedure SetFriction(friction: Single); cdecl;
function GetFriction: Single; cdecl;
procedure ResetFriction; cdecl;
procedure SetRestitution(restitution: Single); cdecl;
function GetRestitution: Single; cdecl;
procedure ResetRestitution; cdecl;
procedure SetTangentSpeed(speed: Single); cdecl;
function GetTangentSpeed: Single; cdecl;
procedure Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
function Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
procedure Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
end;

b2Jacobian = record
linear: b2Vec2;
angularA: Single;
angularB: Single;

class function Create: b2Jacobian; static; cdecl;
end;

b2JointEdge = record
other: b2BodyHandle;
joint: b2JointHandle;
prev: Pb2JointEdge;
next: Pb2JointEdge;

class function Create: b2JointEdge; static; cdecl;
end;

b2JointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;

class function Create: b2JointDef; static; cdecl;
end;

b2JointWrapper = record
FHandle: b2JointHandle;

class operator Implicit(handle: b2JointHandle): b2JointWrapper; overload;
class operator Implicit(wrapper: b2JointWrapper): b2JointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
end;

b2DistanceJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
length: Single;
frequencyHz: Single;
dampingRatio: Single;

class function Create: b2DistanceJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2); cdecl;
end;

b2DistanceJointWrapper = record
FHandle: b2DistanceJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2DistanceJointHandle): b2DistanceJointWrapper; overload;
class operator Implicit(wrapper: b2DistanceJointWrapper): b2DistanceJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
procedure SetLength(length: Single); cdecl;
function GetLength: Single; cdecl;
procedure SetFrequency(hz: Single); cdecl;
function GetFrequency: Single; cdecl;
procedure SetDampingRatio(ratio: Single); cdecl;
function GetDampingRatio: Single; cdecl;
end;

b2FrictionJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
maxForce: Single;
maxTorque: Single;

class function Create: b2FrictionJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
end;

b2FrictionJointWrapper = record
FHandle: b2FrictionJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2FrictionJointHandle): b2FrictionJointWrapper; overload;
class operator Implicit(wrapper: b2FrictionJointWrapper): b2FrictionJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
procedure SetMaxForce(force: Single); cdecl;
function GetMaxForce: Single; cdecl;
procedure SetMaxTorque(torque: Single); cdecl;
function GetMaxTorque: Single; cdecl;
end;

b2GearJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
joint1: b2JointHandle;
joint2: b2JointHandle;
ratio: Single;

class function Create: b2GearJointDef; static; cdecl;
end;

b2GearJointWrapper = record
FHandle: b2GearJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2GearJointHandle): b2GearJointWrapper; overload;
class operator Implicit(wrapper: b2GearJointWrapper): b2GearJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetJoint1: b2JointHandle; cdecl;
function GetJoint2: b2JointHandle; cdecl;
procedure SetRatio(ratio: Single); cdecl;
function GetRatio: Single; cdecl;
end;

b2MotorJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
linearOffset: b2Vec2;
angularOffset: Single;
maxForce: Single;
maxTorque: Single;
correctionFactor: Single;

class function Create: b2MotorJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle); cdecl;
end;

b2MotorJointWrapper = record
FHandle: b2MotorJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2MotorJointHandle): b2MotorJointWrapper; overload;
class operator Implicit(wrapper: b2MotorJointWrapper): b2MotorJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
procedure SetLinearOffset(const [ref] linearOffset: b2Vec2); cdecl;
function GetLinearOffset: Pb2Vec2; cdecl;
procedure SetAngularOffset(angularOffset: Single); cdecl;
function GetAngularOffset: Single; cdecl;
procedure SetMaxForce(force: Single); cdecl;
function GetMaxForce: Single; cdecl;
procedure SetMaxTorque(torque: Single); cdecl;
function GetMaxTorque: Single; cdecl;
procedure SetCorrectionFactor(factor: Single); cdecl;
function GetCorrectionFactor: Single; cdecl;
end;

b2MouseJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
target: b2Vec2;
maxForce: Single;
frequencyHz: Single;
dampingRatio: Single;

class function Create: b2MouseJointDef; static; cdecl;
end;

b2MouseJointWrapper = record
FHandle: b2MouseJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2MouseJointHandle): b2MouseJointWrapper; overload;
class operator Implicit(wrapper: b2MouseJointWrapper): b2MouseJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
procedure SetTarget(const [ref] target: b2Vec2); cdecl;
function GetTarget: Pb2Vec2; cdecl;
procedure SetMaxForce(force: Single); cdecl;
function GetMaxForce: Single; cdecl;
procedure SetFrequency(hz: Single); cdecl;
function GetFrequency: Single; cdecl;
procedure SetDampingRatio(ratio: Single); cdecl;
function GetDampingRatio: Single; cdecl;
end;

b2PrismaticJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
localAxisA: b2Vec2;
referenceAngle: Single;
enableLimit: Boolean;
lowerTranslation: Single;
upperTranslation: Single;
enableMotor: Boolean;
maxMotorForce: Single;
motorSpeed: Single;

class function Create: b2PrismaticJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl;
end;

b2PrismaticJointWrapper = record
FHandle: b2PrismaticJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2PrismaticJointHandle): b2PrismaticJointWrapper; overload;
class operator Implicit(wrapper: b2PrismaticJointWrapper): b2PrismaticJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
function GetLocalAxisA: Pb2Vec2; cdecl;
function GetReferenceAngle: Single; cdecl;
function GetJointTranslation: Single; cdecl;
function GetJointSpeed: Single; cdecl;
function IsLimitEnabled: Boolean; cdecl;
procedure EnableLimit(flag: Boolean); cdecl;
function GetLowerLimit: Single; cdecl;
function GetUpperLimit: Single; cdecl;
procedure SetLimits(lower: Single; upper: Single); cdecl;
function IsMotorEnabled: Boolean; cdecl;
procedure EnableMotor(flag: Boolean); cdecl;
procedure SetMotorSpeed(speed: Single); cdecl;
function GetMotorSpeed: Single; cdecl;
procedure SetMaxMotorForce(force: Single); cdecl;
function GetMaxMotorForce: Single; cdecl;
function GetMotorForce(inv_dt: Single): Single; cdecl;
end;

b2PulleyJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
groundAnchorA: b2Vec2;
groundAnchorB: b2Vec2;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
lengthA: Single;
lengthB: Single;
ratio: Single;

class function Create: b2PulleyJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] groundAnchorA: b2Vec2; const [ref] groundAnchorB: b2Vec2; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2; ratio: Single); cdecl;
end;

b2PulleyJointWrapper = record
FHandle: b2PulleyJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2PulleyJointHandle): b2PulleyJointWrapper; overload;
class operator Implicit(wrapper: b2PulleyJointWrapper): b2PulleyJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetGroundAnchorA: b2Vec2; cdecl;
function GetGroundAnchorB: b2Vec2; cdecl;
function GetLengthA: Single; cdecl;
function GetLengthB: Single; cdecl;
function GetRatio: Single; cdecl;
function GetCurrentLengthA: Single; cdecl;
function GetCurrentLengthB: Single; cdecl;
end;

b2RevoluteJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
referenceAngle: Single;
enableLimit: Boolean;
lowerAngle: Single;
upperAngle: Single;
enableMotor: Boolean;
motorSpeed: Single;
maxMotorTorque: Single;

class function Create: b2RevoluteJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
end;

b2RevoluteJointWrapper = record
FHandle: b2RevoluteJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2RevoluteJointHandle): b2RevoluteJointWrapper; overload;
class operator Implicit(wrapper: b2RevoluteJointWrapper): b2RevoluteJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
function GetReferenceAngle: Single; cdecl;
function GetJointAngle: Single; cdecl;
function GetJointSpeed: Single; cdecl;
function IsLimitEnabled: Boolean; cdecl;
procedure EnableLimit(flag: Boolean); cdecl;
function GetLowerLimit: Single; cdecl;
function GetUpperLimit: Single; cdecl;
procedure SetLimits(lower: Single; upper: Single); cdecl;
function IsMotorEnabled: Boolean; cdecl;
procedure EnableMotor(flag: Boolean); cdecl;
procedure SetMotorSpeed(speed: Single); cdecl;
function GetMotorSpeed: Single; cdecl;
procedure SetMaxMotorTorque(torque: Single); cdecl;
function GetMaxMotorTorque: Single; cdecl;
function GetMotorTorque(inv_dt: Single): Single; cdecl;
end;

b2RopeJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
maxLength: Single;

class function Create: b2RopeJointDef; static; cdecl;
end;

b2RopeJointWrapper = record
FHandle: b2RopeJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2RopeJointHandle): b2RopeJointWrapper; overload;
class operator Implicit(wrapper: b2RopeJointWrapper): b2RopeJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
procedure SetMaxLength(length: Single); cdecl;
function GetMaxLength: Single; cdecl;
function GetLimitState: b2LimitState; cdecl;
end;

b2WeldJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
referenceAngle: Single;
frequencyHz: Single;
dampingRatio: Single;

class function Create: b2WeldJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
end;

b2WeldJointWrapper = record
FHandle: b2WeldJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2WeldJointHandle): b2WeldJointWrapper; overload;
class operator Implicit(wrapper: b2WeldJointWrapper): b2WeldJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
function GetReferenceAngle: Single; cdecl;
procedure SetFrequency(hz: Single); cdecl;
function GetFrequency: Single; cdecl;
procedure SetDampingRatio(ratio: Single); cdecl;
function GetDampingRatio: Single; cdecl;
end;

b2WheelJointDef = record
&type: b2JointType;
userData: Pointer;
bodyA: b2BodyHandle;
bodyB: b2BodyHandle;
collideConnected: Boolean;
localAnchorA: b2Vec2;
localAnchorB: b2Vec2;
localAxisA: b2Vec2;
enableMotor: Boolean;
maxMotorTorque: Single;
motorSpeed: Single;
frequencyHz: Single;
dampingRatio: Single;

class function Create: b2WheelJointDef; static; cdecl;

procedure Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl;
end;

b2WheelJointWrapper = record
FHandle: b2WheelJointHandle;
procedure Destroy; cdecl;

class operator Implicit(handle: b2WheelJointHandle): b2WheelJointWrapper; overload;
class operator Implicit(wrapper: b2WheelJointWrapper): b2WheelJointHandle; overload;
function GetType: b2JointType; cdecl;
function GetBodyA: b2BodyHandle; cdecl;
function GetBodyB: b2BodyHandle; cdecl;
function GetAnchorA: b2Vec2; cdecl;
function GetAnchorB: b2Vec2; cdecl;
function GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
function GetReactionTorque(inv_dt: Single): Single; cdecl;
function GetNext: b2JointHandle; cdecl;
function GetUserData: Pointer; cdecl;
procedure SetUserData(data: Pointer); cdecl;
function IsActive: Boolean; cdecl;
function GetCollideConnected: Boolean; cdecl;
procedure Dump; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
function GetLocalAnchorA: Pb2Vec2; cdecl;
function GetLocalAnchorB: Pb2Vec2; cdecl;
function GetLocalAxisA: Pb2Vec2; cdecl;
function GetJointTranslation: Single; cdecl;
function GetJointSpeed: Single; cdecl;
function IsMotorEnabled: Boolean; cdecl;
procedure EnableMotor(flag: Boolean); cdecl;
procedure SetMotorSpeed(speed: Single); cdecl;
function GetMotorSpeed: Single; cdecl;
procedure SetMaxMotorTorque(torque: Single); cdecl;
function GetMaxMotorTorque: Single; cdecl;
function GetMotorTorque(inv_dt: Single): Single; cdecl;
procedure SetSpringFrequencyHz(hz: Single); cdecl;
function GetSpringFrequencyHz: Single; cdecl;
procedure SetSpringDampingRatio(ratio: Single); cdecl;
function GetSpringDampingRatio: Single; cdecl;
end;

{ ===== Delegate interfaces ===== }

Ib2DestructionListener = interface
['{2DDB25E4-9B99-E391-EF9A-62D1BDC2ABAC}']
procedure SayGoodbye(joint: b2JointHandle); overload; cdecl;
procedure SayGoodbye(fixture: Pb2Fixture); overload; cdecl;
end;

Ib2ContactFilter = interface
['{9BE3CC10-3001-AA1F-3A69-3AF908806081}']
function ShouldCollide(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): Boolean;  cdecl;
end;

Ib2ContactListener = interface
['{457D45B8-C600-3A28-CE8D-7ED8741412D8}']
procedure BeginContact(contact: b2ContactHandle);  cdecl;
procedure EndContact(contact: b2ContactHandle);  cdecl;
procedure PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold);  cdecl;
procedure PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse);  cdecl;
end;

Ib2QueryCallback = interface
['{FA12D838-B458-10AB-2E02-61056056D805}']
function ReportFixture(fixture: Pb2Fixture): Boolean;  cdecl;
end;

Ib2RayCastCallback = interface
['{8E1114AB-969E-8C96-98C4-41096DDCBFA5}']
function ReportFixture(fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single;  cdecl;
end;

{ ===== CreateDestroy of Delegate interfaces ===== }

function Create_b2DestructionListener_delegate(Intf: Ib2DestructionListener): b2DestructionListenerHandle; cdecl;
procedure Destroy_b2DestructionListener_delegate(handle: b2DestructionListenerHandle); cdecl;

function Create_b2ContactFilter_delegate(Intf: Ib2ContactFilter): b2ContactFilterHandle; cdecl;
procedure Destroy_b2ContactFilter_delegate(handle: b2ContactFilterHandle); cdecl;

function Create_b2ContactListener_delegate(Intf: Ib2ContactListener): b2ContactListenerHandle; cdecl;
procedure Destroy_b2ContactListener_delegate(handle: b2ContactListenerHandle); cdecl;

function Create_b2QueryCallback_delegate(Intf: Ib2QueryCallback): b2QueryCallbackHandle; cdecl;
procedure Destroy_b2QueryCallback_delegate(handle: b2QueryCallbackHandle); cdecl;

function Create_b2RayCastCallback_delegate(Intf: Ib2RayCastCallback): b2RayCastCallbackHandle; cdecl;
procedure Destroy_b2RayCastCallback_delegate(handle: b2RayCastCallbackHandle); cdecl;

function b2MixFriction(friction1: Single; friction2: Single): Single; cdecl;
function b2MixRestitution(restitution1: Single; restitution2: Single): Single; cdecl;

implementation

const
{$IFDEF MSWINDOWS}
  LIB_NAME = 'FlatBox2DDyn.dll';
  {$IFDEF WIN64}
    _PU = '';
  {$ELSE}
    _PU = '_';
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  LIB_NAME = 'libFlatBox2D.a';
  _PU = '';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    LIB_NAME = 'libFlatBox2D.a';
  {$ELSE}
    LIB_NAME = 'libFlatBox2DDyn.dylib';
  {$ENDIF}
  {$IFDEF UNDERSCOREIMPORTNAME}
    _PU = '_';
  {$ELSE}
    _PU = '';
  {$ENDIF}
{$ENDIF}

function Create_b2DestructionListener_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2DestructionListener_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2DestructionListener_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2DestructionListener_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



function Create_b2ContactFilter_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2ContactFilter_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2ContactFilter_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2ContactFilter_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



function Create_b2ContactListener_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2ContactListener_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2ContactListener_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2ContactListener_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



function Create_b2QueryCallback_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2QueryCallback_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2QueryCallback_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2QueryCallback_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



function Create_b2RayCastCallback_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2RayCastCallback_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2RayCastCallback_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2RayCastCallback_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



{ ===== Record methods: import and definition ===== }

function b2BodyDef_Create: b2BodyDef; cdecl; external LIB_NAME name _PU + 'b2BodyDef_b2BodyDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2BodyDef.Create: b2BodyDef; cdecl;
begin
  Result := b2BodyDef_Create;
end;


class operator b2BodyWrapper.Implicit(handle: b2BodyHandle): b2BodyWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2BodyWrapper.Implicit(wrapper: b2BodyWrapper): b2BodyHandle;
begin
  Result := wrapper.FHandle;
end;

function b2Body_CreateFixture(_self: b2BodyHandle; def: Pb2FixtureDef): Pb2Fixture; overload; cdecl; external LIB_NAME name _PU + 'b2Body_CreateFixture'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.CreateFixture(def: Pb2FixtureDef): Pb2Fixture; cdecl;
begin
  Result := b2Body_CreateFixture(FHandle, def)
end;

function b2Body_CreateFixture(_self: b2BodyHandle; shape: b2ShapeHandle; density: Single): Pb2Fixture; overload; cdecl; external LIB_NAME name _PU + 'b2Body_CreateFixture2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.CreateFixture(shape: b2ShapeHandle; density: Single): Pb2Fixture; cdecl;
begin
  Result := b2Body_CreateFixture(FHandle, shape, density)
end;

procedure b2Body_DestroyFixture(_self: b2BodyHandle; fixture: Pb2Fixture); cdecl; external LIB_NAME name _PU + 'b2Body_DestroyFixture'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.DestroyFixture(fixture: Pb2Fixture); cdecl;
begin
  b2Body_DestroyFixture(FHandle, fixture)
end;

procedure b2Body_SetTransform(_self: b2BodyHandle; const [ref] position: b2Vec2; angle: Single); cdecl; external LIB_NAME name _PU + 'b2Body_SetTransform'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetTransform(const [ref] position: b2Vec2; angle: Single); cdecl;
begin
  b2Body_SetTransform(FHandle, position, angle)
end;

function b2Body_GetTransform(_self: b2BodyHandle): Pb2Transform; cdecl; external LIB_NAME name _PU + 'b2Body_GetTransform'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetTransform: Pb2Transform; cdecl;
begin
  Result := b2Body_GetTransform(FHandle)
end;

function b2Body_GetPosition(_self: b2BodyHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetPosition'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetPosition: Pb2Vec2; cdecl;
begin
  Result := b2Body_GetPosition(FHandle)
end;

function b2Body_GetAngle(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetAngle: Single; cdecl;
begin
  Result := b2Body_GetAngle(FHandle)
end;

function b2Body_GetWorldCenter(_self: b2BodyHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetWorldCenter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetWorldCenter: Pb2Vec2; cdecl;
begin
  Result := b2Body_GetWorldCenter(FHandle)
end;

function b2Body_GetLocalCenter(_self: b2BodyHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLocalCenter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLocalCenter: Pb2Vec2; cdecl;
begin
  Result := b2Body_GetLocalCenter(FHandle)
end;

procedure b2Body_SetLinearVelocity(_self: b2BodyHandle; const [ref] v: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2Body_SetLinearVelocity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetLinearVelocity(const [ref] v: b2Vec2); cdecl;
begin
  b2Body_SetLinearVelocity(FHandle, v)
end;

function b2Body_GetLinearVelocity(_self: b2BodyHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLinearVelocity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLinearVelocity: Pb2Vec2; cdecl;
begin
  Result := b2Body_GetLinearVelocity(FHandle)
end;

procedure b2Body_SetAngularVelocity(_self: b2BodyHandle; omega: Single); cdecl; external LIB_NAME name _PU + 'b2Body_SetAngularVelocity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetAngularVelocity(omega: Single); cdecl;
begin
  b2Body_SetAngularVelocity(FHandle, omega)
end;

function b2Body_GetAngularVelocity(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetAngularVelocity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetAngularVelocity: Single; cdecl;
begin
  Result := b2Body_GetAngularVelocity(FHandle)
end;

procedure b2Body_ApplyForce(_self: b2BodyHandle; const [ref] force: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_ApplyForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ApplyForce(const [ref] force: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl;
begin
  b2Body_ApplyForce(FHandle, force, point, wake)
end;

procedure b2Body_ApplyForceToCenter(_self: b2BodyHandle; const [ref] force: b2Vec2; wake: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_ApplyForceToCenter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ApplyForceToCenter(const [ref] force: b2Vec2; wake: Boolean); cdecl;
begin
  b2Body_ApplyForceToCenter(FHandle, force, wake)
end;

procedure b2Body_ApplyTorque(_self: b2BodyHandle; torque: Single; wake: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_ApplyTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ApplyTorque(torque: Single; wake: Boolean); cdecl;
begin
  b2Body_ApplyTorque(FHandle, torque, wake)
end;

procedure b2Body_ApplyLinearImpulse(_self: b2BodyHandle; const [ref] impulse: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_ApplyLinearImpulse'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ApplyLinearImpulse(const [ref] impulse: b2Vec2; const [ref] point: b2Vec2; wake: Boolean); cdecl;
begin
  b2Body_ApplyLinearImpulse(FHandle, impulse, point, wake)
end;

procedure b2Body_ApplyAngularImpulse(_self: b2BodyHandle; impulse: Single; wake: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_ApplyAngularImpulse'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ApplyAngularImpulse(impulse: Single; wake: Boolean); cdecl;
begin
  b2Body_ApplyAngularImpulse(FHandle, impulse, wake)
end;

function b2Body_GetMass(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetMass: Single; cdecl;
begin
  Result := b2Body_GetMass(FHandle)
end;

function b2Body_GetInertia(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetInertia'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetInertia: Single; cdecl;
begin
  Result := b2Body_GetInertia(FHandle)
end;

procedure b2Body_GetMassData(_self: b2BodyHandle; data: Pb2MassData); cdecl; external LIB_NAME name _PU + 'b2Body_GetMassData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.GetMassData(data: Pb2MassData); cdecl;
begin
  b2Body_GetMassData(FHandle, data)
end;

procedure b2Body_SetMassData(_self: b2BodyHandle; data: Pb2MassData); cdecl; external LIB_NAME name _PU + 'b2Body_SetMassData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetMassData(data: Pb2MassData); cdecl;
begin
  b2Body_SetMassData(FHandle, data)
end;

procedure b2Body_ResetMassData(_self: b2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2Body_ResetMassData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.ResetMassData; cdecl;
begin
  b2Body_ResetMassData(FHandle)
end;

function b2Body_GetWorldPoint(_self: b2BodyHandle; const [ref] localPoint: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetWorldPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetWorldPoint(const [ref] localPoint: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetWorldPoint(FHandle, localPoint)
end;

function b2Body_GetWorldVector(_self: b2BodyHandle; const [ref] localVector: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetWorldVector'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetWorldVector(const [ref] localVector: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetWorldVector(FHandle, localVector)
end;

function b2Body_GetLocalPoint(_self: b2BodyHandle; const [ref] worldPoint: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLocalPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLocalPoint(const [ref] worldPoint: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetLocalPoint(FHandle, worldPoint)
end;

function b2Body_GetLocalVector(_self: b2BodyHandle; const [ref] worldVector: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLocalVector'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLocalVector(const [ref] worldVector: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetLocalVector(FHandle, worldVector)
end;

function b2Body_GetLinearVelocityFromWorldPoint(_self: b2BodyHandle; const [ref] worldPoint: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLinearVelocityFromWorldPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLinearVelocityFromWorldPoint(const [ref] worldPoint: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetLinearVelocityFromWorldPoint(FHandle, worldPoint)
end;

function b2Body_GetLinearVelocityFromLocalPoint(_self: b2BodyHandle; const [ref] localPoint: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Body_GetLinearVelocityFromLocalPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLinearVelocityFromLocalPoint(const [ref] localPoint: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Body_GetLinearVelocityFromLocalPoint(FHandle, localPoint)
end;

function b2Body_GetLinearDamping(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetLinearDamping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetLinearDamping: Single; cdecl;
begin
  Result := b2Body_GetLinearDamping(FHandle)
end;

procedure b2Body_SetLinearDamping(_self: b2BodyHandle; linearDamping: Single); cdecl; external LIB_NAME name _PU + 'b2Body_SetLinearDamping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetLinearDamping(linearDamping: Single); cdecl;
begin
  b2Body_SetLinearDamping(FHandle, linearDamping)
end;

function b2Body_GetAngularDamping(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetAngularDamping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetAngularDamping: Single; cdecl;
begin
  Result := b2Body_GetAngularDamping(FHandle)
end;

procedure b2Body_SetAngularDamping(_self: b2BodyHandle; angularDamping: Single); cdecl; external LIB_NAME name _PU + 'b2Body_SetAngularDamping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetAngularDamping(angularDamping: Single); cdecl;
begin
  b2Body_SetAngularDamping(FHandle, angularDamping)
end;

function b2Body_GetGravityScale(_self: b2BodyHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Body_GetGravityScale'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetGravityScale: Single; cdecl;
begin
  Result := b2Body_GetGravityScale(FHandle)
end;

procedure b2Body_SetGravityScale(_self: b2BodyHandle; scale: Single); cdecl; external LIB_NAME name _PU + 'b2Body_SetGravityScale'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetGravityScale(scale: Single); cdecl;
begin
  b2Body_SetGravityScale(FHandle, scale)
end;

procedure b2Body_SetType(_self: b2BodyHandle; _type: b2BodyType); cdecl; external LIB_NAME name _PU + 'b2Body_SetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetType(_type: b2BodyType); cdecl;
begin
  b2Body_SetType(FHandle, _type)
end;

function b2Body_GetType(_self: b2BodyHandle): b2BodyType; cdecl; external LIB_NAME name _PU + 'b2Body_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetType: b2BodyType; cdecl;
begin
  Result := b2Body_GetType(FHandle)
end;

procedure b2Body_SetBullet(_self: b2BodyHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_SetBullet'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetBullet(flag: Boolean); cdecl;
begin
  b2Body_SetBullet(FHandle, flag)
end;

function b2Body_IsBullet(_self: b2BodyHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Body_IsBullet'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.IsBullet: Boolean; cdecl;
begin
  Result := b2Body_IsBullet(FHandle)
end;

procedure b2Body_SetSleepingAllowed(_self: b2BodyHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_SetSleepingAllowed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetSleepingAllowed(flag: Boolean); cdecl;
begin
  b2Body_SetSleepingAllowed(FHandle, flag)
end;

function b2Body_IsSleepingAllowed(_self: b2BodyHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Body_IsSleepingAllowed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.IsSleepingAllowed: Boolean; cdecl;
begin
  Result := b2Body_IsSleepingAllowed(FHandle)
end;

procedure b2Body_SetAwake(_self: b2BodyHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_SetAwake'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetAwake(flag: Boolean); cdecl;
begin
  b2Body_SetAwake(FHandle, flag)
end;

function b2Body_IsAwake(_self: b2BodyHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Body_IsAwake'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.IsAwake: Boolean; cdecl;
begin
  Result := b2Body_IsAwake(FHandle)
end;

procedure b2Body_SetActive(_self: b2BodyHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_SetActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetActive(flag: Boolean); cdecl;
begin
  b2Body_SetActive(FHandle, flag)
end;

function b2Body_IsActive(_self: b2BodyHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Body_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2Body_IsActive(FHandle)
end;

procedure b2Body_SetFixedRotation(_self: b2BodyHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Body_SetFixedRotation'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetFixedRotation(flag: Boolean); cdecl;
begin
  b2Body_SetFixedRotation(FHandle, flag)
end;

function b2Body_IsFixedRotation(_self: b2BodyHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Body_IsFixedRotation'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.IsFixedRotation: Boolean; cdecl;
begin
  Result := b2Body_IsFixedRotation(FHandle)
end;

function b2Body_GetFixtureList(_self: b2BodyHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2Body_GetFixtureList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetFixtureList: Pb2Fixture; cdecl;
begin
  Result := b2Body_GetFixtureList(FHandle)
end;

function b2Body_GetJointList(_self: b2BodyHandle): Pb2JointEdge; cdecl; external LIB_NAME name _PU + 'b2Body_GetJointList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetJointList: Pb2JointEdge; cdecl;
begin
  Result := b2Body_GetJointList(FHandle)
end;

function b2Body_GetContactList(_self: b2BodyHandle): Pb2ContactEdge; cdecl; external LIB_NAME name _PU + 'b2Body_GetContactList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetContactList: Pb2ContactEdge; cdecl;
begin
  Result := b2Body_GetContactList(FHandle)
end;

function b2Body_GetNext(_self: b2BodyHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2Body_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetNext: b2BodyHandle; cdecl;
begin
  Result := b2Body_GetNext(FHandle)
end;

function b2Body_GetUserData(_self: b2BodyHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2Body_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2Body_GetUserData(FHandle)
end;

procedure b2Body_SetUserData(_self: b2BodyHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2Body_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2Body_SetUserData(FHandle, data)
end;

function b2Body_GetWorld(_self: b2BodyHandle): b2WorldHandle; cdecl; external LIB_NAME name _PU + 'b2Body_GetWorld'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BodyWrapper.GetWorld: b2WorldHandle; cdecl;
begin
  Result := b2Body_GetWorld(FHandle)
end;

procedure b2Body_Dump(_self: b2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2Body_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BodyWrapper.Dump; cdecl;
begin
  b2Body_Dump(FHandle)
end;


function b2ContactManager_Create: b2ContactManagerHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_b2ContactManager_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactManagerWrapper.Create: b2ContactManagerWrapper; cdecl;
begin
  Result.FHandle := b2ContactManager_Create;
end;

procedure b2ContactManager_Destroy(_self: b2ContactManagerHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManagerWrapper.Destroy; cdecl;
begin
  b2ContactManager_Destroy(FHandle);
end;

class operator b2ContactManagerWrapper.Implicit(handle: b2ContactManagerHandle): b2ContactManagerWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ContactManagerWrapper.Implicit(wrapper: b2ContactManagerWrapper): b2ContactManagerHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2ContactManager_AddPair(_self: b2ContactManagerHandle; proxyUserDataA: Pointer; proxyUserDataB: Pointer); cdecl; external LIB_NAME name _PU + 'b2ContactManager_AddPair'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManagerWrapper.AddPair(proxyUserDataA: Pointer; proxyUserDataB: Pointer); cdecl;
begin
  b2ContactManager_AddPair(FHandle, proxyUserDataA, proxyUserDataB)
end;

procedure b2ContactManager_FindNewContacts(_self: b2ContactManagerHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_FindNewContacts'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManagerWrapper.FindNewContacts; cdecl;
begin
  b2ContactManager_FindNewContacts(FHandle)
end;

procedure b2ContactManager_Destroy_(_self: b2ContactManagerHandle; c: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManagerWrapper.Destroy_(c: b2ContactHandle); cdecl;
begin
  b2ContactManager_Destroy_(FHandle, c)
end;

procedure b2ContactManager_Collide(_self: b2ContactManagerHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Collide'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManagerWrapper.Collide; cdecl;
begin
  b2ContactManager_Collide(FHandle)
end;

function b2ContactManager_Get_m_broadPhase(_self: b2ContactManagerHandle): b2BroadPhaseHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_broadPhase'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_broadPhase(_self: b2ContactManagerHandle; aNewValue: b2BroadPhaseHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_broadPhase'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_broadPhase: b2BroadPhaseHandle; cdecl;
begin
  Result := b2ContactManager_Get_m_broadPhase(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_broadPhase(aNewValue: b2BroadPhaseHandle); cdecl;
begin
  b2ContactManager_Set_m_broadPhase(FHandle, aNewValue);
end;

function b2ContactManager_Get_m_contactList(_self: b2ContactManagerHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_contactList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_contactList(_self: b2ContactManagerHandle; aNewValue: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_contactList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_contactList: b2ContactHandle; cdecl;
begin
  Result := b2ContactManager_Get_m_contactList(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_contactList(aNewValue: b2ContactHandle); cdecl;
begin
  b2ContactManager_Set_m_contactList(FHandle, aNewValue);
end;

function b2ContactManager_Get_m_contactCount(_self: b2ContactManagerHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_contactCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_contactCount(_self: b2ContactManagerHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_contactCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_contactCount: Integer; cdecl;
begin
  Result := b2ContactManager_Get_m_contactCount(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_contactCount(aNewValue: Integer); cdecl;
begin
  b2ContactManager_Set_m_contactCount(FHandle, aNewValue);
end;

function b2ContactManager_Get_m_contactFilter(_self: b2ContactManagerHandle): b2ContactFilterHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_contactFilter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_contactFilter(_self: b2ContactManagerHandle; aNewValue: b2ContactFilterHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_contactFilter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_contactFilter: b2ContactFilterHandle; cdecl;
begin
  Result := b2ContactManager_Get_m_contactFilter(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_contactFilter(aNewValue: b2ContactFilterHandle); cdecl;
begin
  b2ContactManager_Set_m_contactFilter(FHandle, aNewValue);
end;

function b2ContactManager_Get_m_contactListener(_self: b2ContactManagerHandle): b2ContactListenerHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_contactListener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_contactListener(_self: b2ContactManagerHandle; aNewValue: b2ContactListenerHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_contactListener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_contactListener: b2ContactListenerHandle; cdecl;
begin
  Result := b2ContactManager_Get_m_contactListener(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_contactListener(aNewValue: b2ContactListenerHandle); cdecl;
begin
  b2ContactManager_Set_m_contactListener(FHandle, aNewValue);
end;

function b2ContactManager_Get_m_allocator(_self: b2ContactManagerHandle): b2BlockAllocatorHandle; cdecl; external LIB_NAME name _PU + 'b2ContactManager_Get_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactManager_Set_m_allocator(_self: b2ContactManagerHandle; aNewValue: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2ContactManager_Set_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactManagerWrapper.Get_m_allocator: b2BlockAllocatorHandle; cdecl;
begin
  Result := b2ContactManager_Get_m_allocator(FHandle);
end;

procedure b2ContactManagerWrapper.Set_m_allocator(aNewValue: b2BlockAllocatorHandle); cdecl;
begin
  b2ContactManager_Set_m_allocator(FHandle, aNewValue);
end;


function b2Filter_Create: b2Filter; cdecl; external LIB_NAME name _PU + 'b2Filter_b2Filter_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Filter.Create: b2Filter; cdecl;
begin
  Result := b2Filter_Create;
end;


function b2FixtureDef_Create: b2FixtureDef; cdecl; external LIB_NAME name _PU + 'b2FixtureDef_b2FixtureDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2FixtureDef.Create: b2FixtureDef; cdecl;
begin
  Result := b2FixtureDef_Create;
end;


function b2FixtureProxy_Create: b2FixtureProxy; cdecl; external LIB_NAME name _PU + 'b2FixtureProxy_b2FixtureProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2FixtureProxy.Create: b2FixtureProxy; cdecl;
begin
  Result := b2FixtureProxy_Create;
end;


function b2Fixture_GetType(_self: Pb2Fixture): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2Fixture_GetType(@Self)
end;

function b2Fixture_GetShape(_self: Pb2Fixture): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetShape'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetShape: b2ShapeHandle; cdecl;
begin
  Result := b2Fixture_GetShape(@Self)
end;

procedure b2Fixture_SetSensor(_self: Pb2Fixture; sensor: Boolean); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetSensor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetSensor(sensor: Boolean); cdecl;
begin
  b2Fixture_SetSensor(@Self, sensor)
end;

function b2Fixture_IsSensor(_self: Pb2Fixture): Boolean; cdecl; external LIB_NAME name _PU + 'b2Fixture_IsSensor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.IsSensor: Boolean; cdecl;
begin
  Result := b2Fixture_IsSensor(@Self)
end;

procedure b2Fixture_SetFilterData(_self: Pb2Fixture; const [ref] filter: b2Filter); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetFilterData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetFilterData(const [ref] filter: b2Filter); cdecl;
begin
  b2Fixture_SetFilterData(@Self, filter)
end;

function b2Fixture_GetFilterData(_self: Pb2Fixture): Pb2Filter; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetFilterData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetFilterData: Pb2Filter; cdecl;
begin
  Result := b2Fixture_GetFilterData(@Self)
end;

procedure b2Fixture_Refilter(_self: Pb2Fixture); cdecl; external LIB_NAME name _PU + 'b2Fixture_Refilter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.Refilter; cdecl;
begin
  b2Fixture_Refilter(@Self)
end;

function b2Fixture_GetBody(_self: Pb2Fixture): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetBody'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetBody: b2BodyHandle; cdecl;
begin
  Result := b2Fixture_GetBody(@Self)
end;

function b2Fixture_GetNext(_self: Pb2Fixture): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetNext: Pb2Fixture; cdecl;
begin
  Result := b2Fixture_GetNext(@Self)
end;

function b2Fixture_GetUserData(_self: Pb2Fixture): Pointer; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetUserData: Pointer; cdecl;
begin
  Result := b2Fixture_GetUserData(@Self)
end;

procedure b2Fixture_SetUserData(_self: Pb2Fixture; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetUserData(data: Pointer); cdecl;
begin
  b2Fixture_SetUserData(@Self, data)
end;

function b2Fixture_TestPoint(_self: Pb2Fixture; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2Fixture_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.TestPoint(const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2Fixture_TestPoint(@Self, p)
end;

function b2Fixture_RayCast(_self: Pb2Fixture; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2Fixture_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2Fixture_RayCast(@Self, output, input, childIndex)
end;

procedure b2Fixture_GetMassData(_self: Pb2Fixture; massData: Pb2MassData); cdecl; external LIB_NAME name _PU + 'b2Fixture_GetMassData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.GetMassData(massData: Pb2MassData); cdecl;
begin
  b2Fixture_GetMassData(@Self, massData)
end;

procedure b2Fixture_SetDensity(_self: Pb2Fixture; density: Single); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetDensity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetDensity(density: Single); cdecl;
begin
  b2Fixture_SetDensity(@Self, density)
end;

function b2Fixture_GetDensity(_self: Pb2Fixture): Single; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetDensity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetDensity: Single; cdecl;
begin
  Result := b2Fixture_GetDensity(@Self)
end;

function b2Fixture_GetFriction(_self: Pb2Fixture): Single; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetFriction: Single; cdecl;
begin
  Result := b2Fixture_GetFriction(@Self)
end;

procedure b2Fixture_SetFriction(_self: Pb2Fixture; friction: Single); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetFriction(friction: Single); cdecl;
begin
  b2Fixture_SetFriction(@Self, friction)
end;

function b2Fixture_GetRestitution(_self: Pb2Fixture): Single; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetRestitution: Single; cdecl;
begin
  Result := b2Fixture_GetRestitution(@Self)
end;

procedure b2Fixture_SetRestitution(_self: Pb2Fixture; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2Fixture_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.SetRestitution(restitution: Single); cdecl;
begin
  b2Fixture_SetRestitution(@Self, restitution)
end;

function b2Fixture_GetAABB(_self: Pb2Fixture; childIndex: Integer): Pb2AABB; cdecl; external LIB_NAME name _PU + 'b2Fixture_GetAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Fixture.GetAABB(childIndex: Integer): Pb2AABB; cdecl;
begin
  Result := b2Fixture_GetAABB(@Self, childIndex)
end;

procedure b2Fixture_Dump(_self: Pb2Fixture; bodyIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2Fixture_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Fixture.Dump(bodyIndex: Integer); cdecl;
begin
  b2Fixture_Dump(@Self, bodyIndex)
end;


function b2Profile_Create: b2Profile; cdecl; external LIB_NAME name _PU + 'b2Profile_b2Profile'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Profile.Create: b2Profile; cdecl;
begin
  Result := b2Profile_Create;
end;


function b2TimeStep_Create: b2TimeStep; cdecl; external LIB_NAME name _PU + 'b2TimeStep_b2TimeStep'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2TimeStep.Create: b2TimeStep; cdecl;
begin
  Result := b2TimeStep_Create;
end;


function b2Position_Create: b2Position; cdecl; external LIB_NAME name _PU + 'b2Position_b2Position'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Position.Create: b2Position; cdecl;
begin
  Result := b2Position_Create;
end;


function b2Velocity_Create: b2Velocity; cdecl; external LIB_NAME name _PU + 'b2Velocity_b2Velocity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Velocity.Create: b2Velocity; cdecl;
begin
  Result := b2Velocity_Create;
end;


function b2SolverData_Create: b2SolverData; cdecl; external LIB_NAME name _PU + 'b2SolverData_b2SolverData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2SolverData.Create: b2SolverData; cdecl;
begin
  Result := b2SolverData_Create;
end;


function b2Island_Create(bodyCapacity: Integer; contactCapacity: Integer; jointCapacity: Integer; allocator: b2StackAllocatorHandle; listener: b2ContactListenerHandle): b2IslandHandle; cdecl; external LIB_NAME name _PU + 'b2Island_b2Island_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2IslandWrapper.Create(bodyCapacity: Integer; contactCapacity: Integer; jointCapacity: Integer; allocator: b2StackAllocatorHandle; listener: b2ContactListenerHandle): b2IslandWrapper; cdecl;
begin
  Result.FHandle := b2Island_Create(bodyCapacity, contactCapacity, jointCapacity, allocator, listener);
end;

procedure b2Island_Destroy(_self: b2IslandHandle); cdecl; external LIB_NAME name _PU + 'b2Island_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Destroy; cdecl;
begin
  b2Island_Destroy(FHandle);
end;

class operator b2IslandWrapper.Implicit(handle: b2IslandHandle): b2IslandWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2IslandWrapper.Implicit(wrapper: b2IslandWrapper): b2IslandHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2Island_Clear(_self: b2IslandHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Clear'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Clear; cdecl;
begin
  b2Island_Clear(FHandle)
end;

procedure b2Island_Solve(_self: b2IslandHandle; profile: Pb2Profile; const [ref] step: b2TimeStep; const [ref] gravity: b2Vec2; allowSleep: Boolean); cdecl; external LIB_NAME name _PU + 'b2Island_Solve'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Solve(profile: Pb2Profile; const [ref] step: b2TimeStep; const [ref] gravity: b2Vec2; allowSleep: Boolean); cdecl;
begin
  b2Island_Solve(FHandle, profile, step, gravity, allowSleep)
end;

procedure b2Island_SolveTOI(_self: b2IslandHandle; const [ref] subStep: b2TimeStep; toiIndexA: Integer; toiIndexB: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_SolveTOI'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.SolveTOI(const [ref] subStep: b2TimeStep; toiIndexA: Integer; toiIndexB: Integer); cdecl;
begin
  b2Island_SolveTOI(FHandle, subStep, toiIndexA, toiIndexB)
end;

procedure b2Island_Add(_self: b2IslandHandle; body: b2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Add'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Add(body: b2BodyHandle); cdecl;
begin
  b2Island_Add(FHandle, body)
end;

procedure b2Island_Add2(_self: b2IslandHandle; contact: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Add2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Add2(contact: b2ContactHandle); cdecl;
begin
  b2Island_Add2(FHandle, contact)
end;

procedure b2Island_Add3(_self: b2IslandHandle; joint: b2JointHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Add3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Add3(joint: b2JointHandle); cdecl;
begin
  b2Island_Add3(FHandle, joint)
end;

procedure b2Island_Report(_self: b2IslandHandle; constraints: Pb2ContactVelocityConstraint); cdecl; external LIB_NAME name _PU + 'b2Island_Report'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2IslandWrapper.Report(constraints: Pb2ContactVelocityConstraint); cdecl;
begin
  b2Island_Report(FHandle, constraints)
end;

function b2Island_Get_m_allocator(_self: b2IslandHandle): b2StackAllocatorHandle; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_allocator(_self: b2IslandHandle; aNewValue: b2StackAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_allocator: b2StackAllocatorHandle; cdecl;
begin
  Result := b2Island_Get_m_allocator(FHandle);
end;

procedure b2IslandWrapper.Set_m_allocator(aNewValue: b2StackAllocatorHandle); cdecl;
begin
  b2Island_Set_m_allocator(FHandle, aNewValue);
end;

function b2Island_Get_m_listener(_self: b2IslandHandle): b2ContactListenerHandle; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_listener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_listener(_self: b2IslandHandle; aNewValue: b2ContactListenerHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_listener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_listener: b2ContactListenerHandle; cdecl;
begin
  Result := b2Island_Get_m_listener(FHandle);
end;

procedure b2IslandWrapper.Set_m_listener(aNewValue: b2ContactListenerHandle); cdecl;
begin
  b2Island_Set_m_listener(FHandle, aNewValue);
end;

function b2Island_Get_m_bodies(_self: b2IslandHandle): Pb2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_bodies'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_bodies(_self: b2IslandHandle; aNewValue: Pb2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_bodies'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_bodies: Pb2BodyHandle; cdecl;
begin
  Result := b2Island_Get_m_bodies(FHandle);
end;

procedure b2IslandWrapper.Set_m_bodies(aNewValue: Pb2BodyHandle); cdecl;
begin
  b2Island_Set_m_bodies(FHandle, aNewValue);
end;

function b2Island_Get_m_contacts(_self: b2IslandHandle): Pb2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_contacts'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_contacts(_self: b2IslandHandle; aNewValue: Pb2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_contacts'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_contacts: Pb2ContactHandle; cdecl;
begin
  Result := b2Island_Get_m_contacts(FHandle);
end;

procedure b2IslandWrapper.Set_m_contacts(aNewValue: Pb2ContactHandle); cdecl;
begin
  b2Island_Set_m_contacts(FHandle, aNewValue);
end;

function b2Island_Get_m_joints(_self: b2IslandHandle): Pb2JointHandle; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_joints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_joints(_self: b2IslandHandle; aNewValue: Pb2JointHandle); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_joints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_joints: Pb2JointHandle; cdecl;
begin
  Result := b2Island_Get_m_joints(FHandle);
end;

procedure b2IslandWrapper.Set_m_joints(aNewValue: Pb2JointHandle); cdecl;
begin
  b2Island_Set_m_joints(FHandle, aNewValue);
end;

function b2Island_Get_m_positions(_self: b2IslandHandle): Pb2Position; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_positions'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_positions(_self: b2IslandHandle; aNewValue: Pb2Position); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_positions'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_positions: Pb2Position; cdecl;
begin
  Result := b2Island_Get_m_positions(FHandle);
end;

procedure b2IslandWrapper.Set_m_positions(aNewValue: Pb2Position); cdecl;
begin
  b2Island_Set_m_positions(FHandle, aNewValue);
end;

function b2Island_Get_m_velocities(_self: b2IslandHandle): Pb2Velocity; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_velocities'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_velocities(_self: b2IslandHandle; aNewValue: Pb2Velocity); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_velocities'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_velocities: Pb2Velocity; cdecl;
begin
  Result := b2Island_Get_m_velocities(FHandle);
end;

procedure b2IslandWrapper.Set_m_velocities(aNewValue: Pb2Velocity); cdecl;
begin
  b2Island_Set_m_velocities(FHandle, aNewValue);
end;

function b2Island_Get_m_bodyCount(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_bodyCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_bodyCount(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_bodyCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_bodyCount: Integer; cdecl;
begin
  Result := b2Island_Get_m_bodyCount(FHandle);
end;

procedure b2IslandWrapper.Set_m_bodyCount(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_bodyCount(FHandle, aNewValue);
end;

function b2Island_Get_m_jointCount(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_jointCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_jointCount(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_jointCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_jointCount: Integer; cdecl;
begin
  Result := b2Island_Get_m_jointCount(FHandle);
end;

procedure b2IslandWrapper.Set_m_jointCount(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_jointCount(FHandle, aNewValue);
end;

function b2Island_Get_m_contactCount(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_contactCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_contactCount(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_contactCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_contactCount: Integer; cdecl;
begin
  Result := b2Island_Get_m_contactCount(FHandle);
end;

procedure b2IslandWrapper.Set_m_contactCount(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_contactCount(FHandle, aNewValue);
end;

function b2Island_Get_m_bodyCapacity(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_bodyCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_bodyCapacity(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_bodyCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_bodyCapacity: Integer; cdecl;
begin
  Result := b2Island_Get_m_bodyCapacity(FHandle);
end;

procedure b2IslandWrapper.Set_m_bodyCapacity(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_bodyCapacity(FHandle, aNewValue);
end;

function b2Island_Get_m_contactCapacity(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_contactCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_contactCapacity(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_contactCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_contactCapacity: Integer; cdecl;
begin
  Result := b2Island_Get_m_contactCapacity(FHandle);
end;

procedure b2IslandWrapper.Set_m_contactCapacity(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_contactCapacity(FHandle, aNewValue);
end;

function b2Island_Get_m_jointCapacity(_self: b2IslandHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Island_Get_m_jointCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Island_Set_m_jointCapacity(_self: b2IslandHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2Island_Set_m_jointCapacity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IslandWrapper.Get_m_jointCapacity: Integer; cdecl;
begin
  Result := b2Island_Get_m_jointCapacity(FHandle);
end;

procedure b2IslandWrapper.Set_m_jointCapacity(aNewValue: Integer); cdecl;
begin
  b2Island_Set_m_jointCapacity(FHandle, aNewValue);
end;


class operator b2DestructionListenerWrapper.Implicit(handle: b2DestructionListenerHandle): b2DestructionListenerWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2DestructionListenerWrapper.Implicit(wrapper: b2DestructionListenerWrapper): b2DestructionListenerHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2DestructionListener_SayGoodbye(_self: b2DestructionListenerHandle; joint: b2JointHandle); overload; cdecl; external LIB_NAME name _PU + 'b2DestructionListener_SayGoodbye'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DestructionListenerWrapper.SayGoodbye(joint: b2JointHandle); cdecl;
begin
  b2DestructionListener_SayGoodbye(FHandle, joint)
end;

procedure b2DestructionListener_SayGoodbye(_self: b2DestructionListenerHandle; fixture: Pb2Fixture); overload; cdecl; external LIB_NAME name _PU + 'b2DestructionListener_SayGoodbye2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DestructionListenerWrapper.SayGoodbye(fixture: Pb2Fixture); cdecl;
begin
  b2DestructionListener_SayGoodbye(FHandle, fixture)
end;


function b2ContactFilter_Create: b2ContactFilterHandle; cdecl; external LIB_NAME name _PU + 'b2ContactFilter_b2ContactFilter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactFilterWrapper.Create: b2ContactFilterWrapper; cdecl;
begin
  Result.FHandle := b2ContactFilter_Create;
end;

procedure b2ContactFilter_Destroy(_self: b2ContactFilterHandle); cdecl; external LIB_NAME name _PU + 'b2ContactFilter_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactFilterWrapper.Destroy; cdecl;
begin
  b2ContactFilter_Destroy(FHandle);
end;

class operator b2ContactFilterWrapper.Implicit(handle: b2ContactFilterHandle): b2ContactFilterWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ContactFilterWrapper.Implicit(wrapper: b2ContactFilterWrapper): b2ContactFilterHandle;
begin
  Result := wrapper.FHandle;
end;

function b2ContactFilter_ShouldCollide(_self: b2ContactFilterHandle; fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): Boolean; cdecl; external LIB_NAME name _PU + 'b2ContactFilter_ShouldCollide'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactFilterWrapper.ShouldCollide(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): Boolean; cdecl;
begin
  Result := b2ContactFilter_ShouldCollide(FHandle, fixtureA, fixtureB)
end;


function b2ContactImpulse_Create: b2ContactImpulse; cdecl; external LIB_NAME name _PU + 'b2ContactImpulse_b2ContactImpulse'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactImpulse.Create: b2ContactImpulse; cdecl;
begin
  Result := b2ContactImpulse_Create;
end;


function b2ContactListener_Create: b2ContactListenerHandle; cdecl; external LIB_NAME name _PU + 'b2ContactListener_b2ContactListener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactListenerWrapper.Create: b2ContactListenerWrapper; cdecl;
begin
  Result.FHandle := b2ContactListener_Create;
end;

procedure b2ContactListener_Destroy(_self: b2ContactListenerHandle); cdecl; external LIB_NAME name _PU + 'b2ContactListener_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactListenerWrapper.Destroy; cdecl;
begin
  b2ContactListener_Destroy(FHandle);
end;

class operator b2ContactListenerWrapper.Implicit(handle: b2ContactListenerHandle): b2ContactListenerWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ContactListenerWrapper.Implicit(wrapper: b2ContactListenerWrapper): b2ContactListenerHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2ContactListener_BeginContact(_self: b2ContactListenerHandle; contact: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2ContactListener_BeginContact'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactListenerWrapper.BeginContact(contact: b2ContactHandle); cdecl;
begin
  b2ContactListener_BeginContact(FHandle, contact)
end;

procedure b2ContactListener_EndContact(_self: b2ContactListenerHandle; contact: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2ContactListener_EndContact'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactListenerWrapper.EndContact(contact: b2ContactHandle); cdecl;
begin
  b2ContactListener_EndContact(FHandle, contact)
end;

procedure b2ContactListener_PreSolve(_self: b2ContactListenerHandle; contact: b2ContactHandle; oldManifold: Pb2Manifold); cdecl; external LIB_NAME name _PU + 'b2ContactListener_PreSolve'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactListenerWrapper.PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold); cdecl;
begin
  b2ContactListener_PreSolve(FHandle, contact, oldManifold)
end;

procedure b2ContactListener_PostSolve(_self: b2ContactListenerHandle; contact: b2ContactHandle; impulse: Pb2ContactImpulse); cdecl; external LIB_NAME name _PU + 'b2ContactListener_PostSolve'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactListenerWrapper.PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse); cdecl;
begin
  b2ContactListener_PostSolve(FHandle, contact, impulse)
end;


class operator b2QueryCallbackWrapper.Implicit(handle: b2QueryCallbackHandle): b2QueryCallbackWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2QueryCallbackWrapper.Implicit(wrapper: b2QueryCallbackWrapper): b2QueryCallbackHandle;
begin
  Result := wrapper.FHandle;
end;

function b2QueryCallback_ReportFixture(_self: b2QueryCallbackHandle; fixture: Pb2Fixture): Boolean; cdecl; external LIB_NAME name _PU + 'b2QueryCallback_ReportFixture'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2QueryCallbackWrapper.ReportFixture(fixture: Pb2Fixture): Boolean; cdecl;
begin
  Result := b2QueryCallback_ReportFixture(FHandle, fixture)
end;


class operator b2RayCastCallbackWrapper.Implicit(handle: b2RayCastCallbackHandle): b2RayCastCallbackWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2RayCastCallbackWrapper.Implicit(wrapper: b2RayCastCallbackWrapper): b2RayCastCallbackHandle;
begin
  Result := wrapper.FHandle;
end;

function b2RayCastCallback_ReportFixture(_self: b2RayCastCallbackHandle; fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single; cdecl; external LIB_NAME name _PU + 'b2RayCastCallback_ReportFixture'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RayCastCallbackWrapper.ReportFixture(fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single; cdecl;
begin
  Result := b2RayCastCallback_ReportFixture(FHandle, fixture, point, normal, fraction)
end;


function b2World_Create(const [ref] gravity: b2Vec2): b2WorldHandle; cdecl; external LIB_NAME name _PU + 'b2World_b2World_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2WorldWrapper.Create(const [ref] gravity: b2Vec2): b2WorldWrapper; cdecl;
begin
  Result.FHandle := b2World_Create(gravity);
end;

procedure b2World_Destroy(_self: b2WorldHandle); cdecl; external LIB_NAME name _PU + 'b2World_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.Destroy; cdecl;
begin
  b2World_Destroy(FHandle);
end;

class operator b2WorldWrapper.Implicit(handle: b2WorldHandle): b2WorldWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2WorldWrapper.Implicit(wrapper: b2WorldWrapper): b2WorldHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2World_SetDestructionListener(_self: b2WorldHandle; listener: b2DestructionListenerHandle); cdecl; external LIB_NAME name _PU + 'b2World_SetDestructionListener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetDestructionListener(listener: b2DestructionListenerHandle); cdecl;
begin
  b2World_SetDestructionListener(FHandle, listener)
end;

procedure b2World_SetContactFilter(_self: b2WorldHandle; filter: b2ContactFilterHandle); cdecl; external LIB_NAME name _PU + 'b2World_SetContactFilter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetContactFilter(filter: b2ContactFilterHandle); cdecl;
begin
  b2World_SetContactFilter(FHandle, filter)
end;

procedure b2World_SetContactListener(_self: b2WorldHandle; listener: b2ContactListenerHandle); cdecl; external LIB_NAME name _PU + 'b2World_SetContactListener'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetContactListener(listener: b2ContactListenerHandle); cdecl;
begin
  b2World_SetContactListener(FHandle, listener)
end;

procedure b2World_SetDebugDraw(_self: b2WorldHandle; debugDraw: b2DrawHandle); cdecl; external LIB_NAME name _PU + 'b2World_SetDebugDraw'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetDebugDraw(debugDraw: b2DrawHandle); cdecl;
begin
  b2World_SetDebugDraw(FHandle, debugDraw)
end;

function b2World_CreateBody(_self: b2WorldHandle; def: Pb2BodyDef): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2World_CreateBody'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.CreateBody(def: Pb2BodyDef): b2BodyHandle; cdecl;
begin
  Result := b2World_CreateBody(FHandle, def)
end;

procedure b2World_DestroyBody(_self: b2WorldHandle; body: b2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2World_DestroyBody'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.DestroyBody(body: b2BodyHandle); cdecl;
begin
  b2World_DestroyBody(FHandle, body)
end;

function b2World_CreateJoint(_self: b2WorldHandle; def: Pb2JointDef): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2World_CreateJoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.CreateJoint(def: Pb2JointDef): b2JointHandle; cdecl;
begin
  Result := b2World_CreateJoint(FHandle, def)
end;

procedure b2World_DestroyJoint(_self: b2WorldHandle; joint: b2JointHandle); cdecl; external LIB_NAME name _PU + 'b2World_DestroyJoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.DestroyJoint(joint: b2JointHandle); cdecl;
begin
  b2World_DestroyJoint(FHandle, joint)
end;

procedure b2World_Step(_self: b2WorldHandle; timeStep: Single; velocityIterations: Integer; positionIterations: Integer); cdecl; external LIB_NAME name _PU + 'b2World_Step'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.Step(timeStep: Single; velocityIterations: Integer; positionIterations: Integer); cdecl;
begin
  b2World_Step(FHandle, timeStep, velocityIterations, positionIterations)
end;

procedure b2World_ClearForces(_self: b2WorldHandle); cdecl; external LIB_NAME name _PU + 'b2World_ClearForces'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.ClearForces; cdecl;
begin
  b2World_ClearForces(FHandle)
end;

procedure b2World_DrawDebugData(_self: b2WorldHandle); cdecl; external LIB_NAME name _PU + 'b2World_DrawDebugData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.DrawDebugData; cdecl;
begin
  b2World_DrawDebugData(FHandle)
end;

procedure b2World_QueryAABB(_self: b2WorldHandle; callback: b2QueryCallbackHandle; const [ref] aabb: b2AABB); cdecl; external LIB_NAME name _PU + 'b2World_QueryAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.QueryAABB(callback: b2QueryCallbackHandle; const [ref] aabb: b2AABB); cdecl;
begin
  b2World_QueryAABB(FHandle, callback, aabb)
end;

procedure b2World_RayCast(_self: b2WorldHandle; callback: b2RayCastCallbackHandle; const [ref] point1: b2Vec2; const [ref] point2: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2World_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.RayCast(callback: b2RayCastCallbackHandle; const [ref] point1: b2Vec2; const [ref] point2: b2Vec2); cdecl;
begin
  b2World_RayCast(FHandle, callback, point1, point2)
end;

function b2World_GetBodyList(_self: b2WorldHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2World_GetBodyList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetBodyList: b2BodyHandle; cdecl;
begin
  Result := b2World_GetBodyList(FHandle)
end;

function b2World_GetJointList(_self: b2WorldHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2World_GetJointList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetJointList: b2JointHandle; cdecl;
begin
  Result := b2World_GetJointList(FHandle)
end;

function b2World_GetContactList(_self: b2WorldHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2World_GetContactList'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetContactList: b2ContactHandle; cdecl;
begin
  Result := b2World_GetContactList(FHandle)
end;

procedure b2World_SetAllowSleeping(_self: b2WorldHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2World_SetAllowSleeping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetAllowSleeping(flag: Boolean); cdecl;
begin
  b2World_SetAllowSleeping(FHandle, flag)
end;

function b2World_GetAllowSleeping(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_GetAllowSleeping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetAllowSleeping: Boolean; cdecl;
begin
  Result := b2World_GetAllowSleeping(FHandle)
end;

procedure b2World_SetWarmStarting(_self: b2WorldHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2World_SetWarmStarting'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetWarmStarting(flag: Boolean); cdecl;
begin
  b2World_SetWarmStarting(FHandle, flag)
end;

function b2World_GetWarmStarting(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_GetWarmStarting'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetWarmStarting: Boolean; cdecl;
begin
  Result := b2World_GetWarmStarting(FHandle)
end;

procedure b2World_SetContinuousPhysics(_self: b2WorldHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2World_SetContinuousPhysics'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetContinuousPhysics(flag: Boolean); cdecl;
begin
  b2World_SetContinuousPhysics(FHandle, flag)
end;

function b2World_GetContinuousPhysics(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_GetContinuousPhysics'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetContinuousPhysics: Boolean; cdecl;
begin
  Result := b2World_GetContinuousPhysics(FHandle)
end;

procedure b2World_SetSubStepping(_self: b2WorldHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2World_SetSubStepping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetSubStepping(flag: Boolean); cdecl;
begin
  b2World_SetSubStepping(FHandle, flag)
end;

function b2World_GetSubStepping(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_GetSubStepping'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetSubStepping: Boolean; cdecl;
begin
  Result := b2World_GetSubStepping(FHandle)
end;

function b2World_GetProxyCount(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetProxyCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetProxyCount: Integer; cdecl;
begin
  Result := b2World_GetProxyCount(FHandle)
end;

function b2World_GetBodyCount(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetBodyCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetBodyCount: Integer; cdecl;
begin
  Result := b2World_GetBodyCount(FHandle)
end;

function b2World_GetJointCount(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetJointCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetJointCount: Integer; cdecl;
begin
  Result := b2World_GetJointCount(FHandle)
end;

function b2World_GetContactCount(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetContactCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetContactCount: Integer; cdecl;
begin
  Result := b2World_GetContactCount(FHandle)
end;

function b2World_GetTreeHeight(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetTreeHeight'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetTreeHeight: Integer; cdecl;
begin
  Result := b2World_GetTreeHeight(FHandle)
end;

function b2World_GetTreeBalance(_self: b2WorldHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2World_GetTreeBalance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetTreeBalance: Integer; cdecl;
begin
  Result := b2World_GetTreeBalance(FHandle)
end;

function b2World_GetTreeQuality(_self: b2WorldHandle): Single; cdecl; external LIB_NAME name _PU + 'b2World_GetTreeQuality'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetTreeQuality: Single; cdecl;
begin
  Result := b2World_GetTreeQuality(FHandle)
end;

procedure b2World_SetGravity(_self: b2WorldHandle; const [ref] gravity: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2World_SetGravity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetGravity(const [ref] gravity: b2Vec2); cdecl;
begin
  b2World_SetGravity(FHandle, gravity)
end;

function b2World_GetGravity(_self: b2WorldHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2World_GetGravity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetGravity: b2Vec2; cdecl;
begin
  Result := b2World_GetGravity(FHandle)
end;

function b2World_IsLocked(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_IsLocked'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.IsLocked: Boolean; cdecl;
begin
  Result := b2World_IsLocked(FHandle)
end;

procedure b2World_SetAutoClearForces(_self: b2WorldHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2World_SetAutoClearForces'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.SetAutoClearForces(flag: Boolean); cdecl;
begin
  b2World_SetAutoClearForces(FHandle, flag)
end;

function b2World_GetAutoClearForces(_self: b2WorldHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2World_GetAutoClearForces'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetAutoClearForces: Boolean; cdecl;
begin
  Result := b2World_GetAutoClearForces(FHandle)
end;

procedure b2World_ShiftOrigin(_self: b2WorldHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2World_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2World_ShiftOrigin(FHandle, newOrigin)
end;

function b2World_GetContactManager(_self: b2WorldHandle): b2ContactManagerHandle; cdecl; external LIB_NAME name _PU + 'b2World_GetContactManager'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetContactManager: b2ContactManagerHandle; cdecl;
begin
  Result := b2World_GetContactManager(FHandle)
end;

function b2World_GetProfile(_self: b2WorldHandle): Pb2Profile; cdecl; external LIB_NAME name _PU + 'b2World_GetProfile'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WorldWrapper.GetProfile: Pb2Profile; cdecl;
begin
  Result := b2World_GetProfile(FHandle)
end;

procedure b2World_Dump(_self: b2WorldHandle); cdecl; external LIB_NAME name _PU + 'b2World_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldWrapper.Dump; cdecl;
begin
  b2World_Dump(FHandle)
end;


function b2ContactRegister_Create: b2ContactRegister; cdecl; external LIB_NAME name _PU + 'b2ContactRegister_b2ContactRegister'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactRegister.Create: b2ContactRegister; cdecl;
begin
  Result := b2ContactRegister_Create;
end;


function b2ContactEdge_Create: b2ContactEdge; cdecl; external LIB_NAME name _PU + 'b2ContactEdge_b2ContactEdge'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactEdge.Create: b2ContactEdge; cdecl;
begin
  Result := b2ContactEdge_Create;
end;


class operator b2ContactWrapper.Implicit(handle: b2ContactHandle): b2ContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ContactWrapper.Implicit(wrapper: b2ContactWrapper): b2ContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2Contact_GetManifold(_self: b2ContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2Contact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2Contact_GetManifold(FHandle)
end;

procedure b2Contact_GetWorldManifold(_self: b2ContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2Contact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2Contact_GetWorldManifold(FHandle, worldManifold)
end;

function b2Contact_IsTouching(_self: b2ContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Contact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2Contact_IsTouching(FHandle)
end;

procedure b2Contact_SetEnabled(_self: b2ContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2Contact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2Contact_SetEnabled(FHandle, flag)
end;

function b2Contact_IsEnabled(_self: b2ContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Contact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2Contact_IsEnabled(FHandle)
end;

function b2Contact_GetNext(_self: b2ContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2Contact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2Contact_GetNext(FHandle)
end;

function b2Contact_GetFixtureA(_self: b2ContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2Contact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2Contact_GetFixtureA(FHandle)
end;

function b2Contact_GetChildIndexA(_self: b2ContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Contact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2Contact_GetChildIndexA(FHandle)
end;

function b2Contact_GetFixtureB(_self: b2ContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2Contact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2Contact_GetFixtureB(FHandle)
end;

function b2Contact_GetChildIndexB(_self: b2ContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Contact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2Contact_GetChildIndexB(FHandle)
end;

procedure b2Contact_SetFriction(_self: b2ContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2Contact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2Contact_SetFriction(FHandle, friction)
end;

function b2Contact_GetFriction(_self: b2ContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Contact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2Contact_GetFriction(FHandle)
end;

procedure b2Contact_ResetFriction(_self: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2Contact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.ResetFriction; cdecl;
begin
  b2Contact_ResetFriction(FHandle)
end;

procedure b2Contact_SetRestitution(_self: b2ContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2Contact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2Contact_SetRestitution(FHandle, restitution)
end;

function b2Contact_GetRestitution(_self: b2ContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Contact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2Contact_GetRestitution(FHandle)
end;

procedure b2Contact_ResetRestitution(_self: b2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2Contact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.ResetRestitution; cdecl;
begin
  b2Contact_ResetRestitution(FHandle)
end;

procedure b2Contact_SetTangentSpeed(_self: b2ContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2Contact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2Contact_SetTangentSpeed(FHandle, speed)
end;

function b2Contact_GetTangentSpeed(_self: b2ContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Contact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2Contact_GetTangentSpeed(FHandle)
end;

procedure b2Contact_Evaluate(_self: b2ContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2Contact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2Contact_Evaluate(FHandle, manifold, xfA, xfB)
end;


function b2ChainAndCircleContact_Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndCircleContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_b2ChainAndCircleContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ChainAndCircleContactWrapper.Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndCircleContactWrapper; cdecl;
begin
  Result.FHandle := b2ChainAndCircleContact_Create(fixtureA, indexA, fixtureB, indexB);
end;

procedure b2ChainAndCircleContact_Destroy(_self: b2ChainAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.Destroy; cdecl;
begin
  b2ChainAndCircleContact_Destroy(FHandle);
end;

class operator b2ChainAndCircleContactWrapper.Implicit(handle: b2ChainAndCircleContactHandle): b2ChainAndCircleContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ChainAndCircleContactWrapper.Implicit(wrapper: b2ChainAndCircleContactWrapper): b2ChainAndCircleContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2ChainAndCircleContact_GetManifold(_self: b2ChainAndCircleContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2ChainAndCircleContact_GetManifold(FHandle)
end;

procedure b2ChainAndCircleContact_GetWorldManifold(_self: b2ChainAndCircleContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2ChainAndCircleContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2ChainAndCircleContact_IsTouching(_self: b2ChainAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2ChainAndCircleContact_IsTouching(FHandle)
end;

procedure b2ChainAndCircleContact_SetEnabled(_self: b2ChainAndCircleContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2ChainAndCircleContact_SetEnabled(FHandle, flag)
end;

function b2ChainAndCircleContact_IsEnabled(_self: b2ChainAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2ChainAndCircleContact_IsEnabled(FHandle)
end;

function b2ChainAndCircleContact_GetNext(_self: b2ChainAndCircleContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2ChainAndCircleContact_GetNext(FHandle)
end;

function b2ChainAndCircleContact_GetFixtureA(_self: b2ChainAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2ChainAndCircleContact_GetFixtureA(FHandle)
end;

function b2ChainAndCircleContact_GetChildIndexA(_self: b2ChainAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2ChainAndCircleContact_GetChildIndexA(FHandle)
end;

function b2ChainAndCircleContact_GetFixtureB(_self: b2ChainAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2ChainAndCircleContact_GetFixtureB(FHandle)
end;

function b2ChainAndCircleContact_GetChildIndexB(_self: b2ChainAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2ChainAndCircleContact_GetChildIndexB(FHandle)
end;

procedure b2ChainAndCircleContact_SetFriction(_self: b2ChainAndCircleContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2ChainAndCircleContact_SetFriction(FHandle, friction)
end;

function b2ChainAndCircleContact_GetFriction(_self: b2ChainAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2ChainAndCircleContact_GetFriction(FHandle)
end;

procedure b2ChainAndCircleContact_ResetFriction(_self: b2ChainAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.ResetFriction; cdecl;
begin
  b2ChainAndCircleContact_ResetFriction(FHandle)
end;

procedure b2ChainAndCircleContact_SetRestitution(_self: b2ChainAndCircleContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2ChainAndCircleContact_SetRestitution(FHandle, restitution)
end;

function b2ChainAndCircleContact_GetRestitution(_self: b2ChainAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2ChainAndCircleContact_GetRestitution(FHandle)
end;

procedure b2ChainAndCircleContact_ResetRestitution(_self: b2ChainAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.ResetRestitution; cdecl;
begin
  b2ChainAndCircleContact_ResetRestitution(FHandle)
end;

procedure b2ChainAndCircleContact_SetTangentSpeed(_self: b2ChainAndCircleContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2ChainAndCircleContact_SetTangentSpeed(FHandle, speed)
end;

function b2ChainAndCircleContact_GetTangentSpeed(_self: b2ChainAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2ChainAndCircleContact_GetTangentSpeed(FHandle)
end;

procedure b2ChainAndCircleContact_Evaluate(_self: b2ChainAndCircleContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2ChainAndCircleContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2ChainAndCircleContact_Create_(_self: b2ChainAndCircleContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndCircleContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2ChainAndCircleContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2ChainAndCircleContact_Destroy_(_self: b2ChainAndCircleContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndCircleContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndCircleContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2ChainAndCircleContact_Destroy_(FHandle, contact, allocator)
end;


function b2ChainAndPolygonContact_Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndPolygonContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_b2ChainAndPolygonContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ChainAndPolygonContactWrapper.Create(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer): b2ChainAndPolygonContactWrapper; cdecl;
begin
  Result.FHandle := b2ChainAndPolygonContact_Create(fixtureA, indexA, fixtureB, indexB);
end;

procedure b2ChainAndPolygonContact_Destroy(_self: b2ChainAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.Destroy; cdecl;
begin
  b2ChainAndPolygonContact_Destroy(FHandle);
end;

class operator b2ChainAndPolygonContactWrapper.Implicit(handle: b2ChainAndPolygonContactHandle): b2ChainAndPolygonContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ChainAndPolygonContactWrapper.Implicit(wrapper: b2ChainAndPolygonContactWrapper): b2ChainAndPolygonContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2ChainAndPolygonContact_GetManifold(_self: b2ChainAndPolygonContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetManifold(FHandle)
end;

procedure b2ChainAndPolygonContact_GetWorldManifold(_self: b2ChainAndPolygonContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2ChainAndPolygonContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2ChainAndPolygonContact_IsTouching(_self: b2ChainAndPolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2ChainAndPolygonContact_IsTouching(FHandle)
end;

procedure b2ChainAndPolygonContact_SetEnabled(_self: b2ChainAndPolygonContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2ChainAndPolygonContact_SetEnabled(FHandle, flag)
end;

function b2ChainAndPolygonContact_IsEnabled(_self: b2ChainAndPolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2ChainAndPolygonContact_IsEnabled(FHandle)
end;

function b2ChainAndPolygonContact_GetNext(_self: b2ChainAndPolygonContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetNext(FHandle)
end;

function b2ChainAndPolygonContact_GetFixtureA(_self: b2ChainAndPolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetFixtureA(FHandle)
end;

function b2ChainAndPolygonContact_GetChildIndexA(_self: b2ChainAndPolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetChildIndexA(FHandle)
end;

function b2ChainAndPolygonContact_GetFixtureB(_self: b2ChainAndPolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetFixtureB(FHandle)
end;

function b2ChainAndPolygonContact_GetChildIndexB(_self: b2ChainAndPolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetChildIndexB(FHandle)
end;

procedure b2ChainAndPolygonContact_SetFriction(_self: b2ChainAndPolygonContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2ChainAndPolygonContact_SetFriction(FHandle, friction)
end;

function b2ChainAndPolygonContact_GetFriction(_self: b2ChainAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetFriction(FHandle)
end;

procedure b2ChainAndPolygonContact_ResetFriction(_self: b2ChainAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.ResetFriction; cdecl;
begin
  b2ChainAndPolygonContact_ResetFriction(FHandle)
end;

procedure b2ChainAndPolygonContact_SetRestitution(_self: b2ChainAndPolygonContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2ChainAndPolygonContact_SetRestitution(FHandle, restitution)
end;

function b2ChainAndPolygonContact_GetRestitution(_self: b2ChainAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetRestitution(FHandle)
end;

procedure b2ChainAndPolygonContact_ResetRestitution(_self: b2ChainAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.ResetRestitution; cdecl;
begin
  b2ChainAndPolygonContact_ResetRestitution(FHandle)
end;

procedure b2ChainAndPolygonContact_SetTangentSpeed(_self: b2ChainAndPolygonContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2ChainAndPolygonContact_SetTangentSpeed(FHandle, speed)
end;

function b2ChainAndPolygonContact_GetTangentSpeed(_self: b2ChainAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2ChainAndPolygonContact_GetTangentSpeed(FHandle)
end;

procedure b2ChainAndPolygonContact_Evaluate(_self: b2ChainAndPolygonContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2ChainAndPolygonContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2ChainAndPolygonContact_Create_(_self: b2ChainAndPolygonContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainAndPolygonContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2ChainAndPolygonContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2ChainAndPolygonContact_Destroy_(_self: b2ChainAndPolygonContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2ChainAndPolygonContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainAndPolygonContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2ChainAndPolygonContact_Destroy_(FHandle, contact, allocator)
end;


function b2CircleContact_Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2CircleContactHandle; cdecl; external LIB_NAME name _PU + 'b2CircleContact_b2CircleContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2CircleContactWrapper.Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2CircleContactWrapper; cdecl;
begin
  Result.FHandle := b2CircleContact_Create(fixtureA, fixtureB);
end;

procedure b2CircleContact_Destroy(_self: b2CircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2CircleContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.Destroy; cdecl;
begin
  b2CircleContact_Destroy(FHandle);
end;

class operator b2CircleContactWrapper.Implicit(handle: b2CircleContactHandle): b2CircleContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2CircleContactWrapper.Implicit(wrapper: b2CircleContactWrapper): b2CircleContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2CircleContact_GetManifold(_self: b2CircleContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2CircleContact_GetManifold(FHandle)
end;

procedure b2CircleContact_GetWorldManifold(_self: b2CircleContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2CircleContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2CircleContact_IsTouching(_self: b2CircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2CircleContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2CircleContact_IsTouching(FHandle)
end;

procedure b2CircleContact_SetEnabled(_self: b2CircleContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2CircleContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2CircleContact_SetEnabled(FHandle, flag)
end;

function b2CircleContact_IsEnabled(_self: b2CircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2CircleContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2CircleContact_IsEnabled(FHandle)
end;

function b2CircleContact_GetNext(_self: b2CircleContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2CircleContact_GetNext(FHandle)
end;

function b2CircleContact_GetFixtureA(_self: b2CircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2CircleContact_GetFixtureA(FHandle)
end;

function b2CircleContact_GetChildIndexA(_self: b2CircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2CircleContact_GetChildIndexA(FHandle)
end;

function b2CircleContact_GetFixtureB(_self: b2CircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2CircleContact_GetFixtureB(FHandle)
end;

function b2CircleContact_GetChildIndexB(_self: b2CircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2CircleContact_GetChildIndexB(FHandle)
end;

procedure b2CircleContact_SetFriction(_self: b2CircleContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2CircleContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2CircleContact_SetFriction(FHandle, friction)
end;

function b2CircleContact_GetFriction(_self: b2CircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2CircleContact_GetFriction(FHandle)
end;

procedure b2CircleContact_ResetFriction(_self: b2CircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2CircleContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.ResetFriction; cdecl;
begin
  b2CircleContact_ResetFriction(FHandle)
end;

procedure b2CircleContact_SetRestitution(_self: b2CircleContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2CircleContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2CircleContact_SetRestitution(FHandle, restitution)
end;

function b2CircleContact_GetRestitution(_self: b2CircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2CircleContact_GetRestitution(FHandle)
end;

procedure b2CircleContact_ResetRestitution(_self: b2CircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2CircleContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.ResetRestitution; cdecl;
begin
  b2CircleContact_ResetRestitution(FHandle)
end;

procedure b2CircleContact_SetTangentSpeed(_self: b2CircleContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2CircleContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2CircleContact_SetTangentSpeed(FHandle, speed)
end;

function b2CircleContact_GetTangentSpeed(_self: b2CircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2CircleContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2CircleContact_GetTangentSpeed(FHandle)
end;

procedure b2CircleContact_Evaluate(_self: b2CircleContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2CircleContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2CircleContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2CircleContact_Create_(_self: b2CircleContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2CircleContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2CircleContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2CircleContact_Destroy_(_self: b2CircleContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2CircleContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2CircleContact_Destroy_(FHandle, contact, allocator)
end;


function b2VelocityConstraintPoint_Create: b2VelocityConstraintPoint; cdecl; external LIB_NAME name _PU + 'b2VelocityConstraintPoint_b2VelocityConstraintPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2VelocityConstraintPoint.Create: b2VelocityConstraintPoint; cdecl;
begin
  Result := b2VelocityConstraintPoint_Create;
end;


function b2ContactVelocityConstraint_Create: b2ContactVelocityConstraint; cdecl; external LIB_NAME name _PU + 'b2ContactVelocityConstraint_b2ContactVelocityConstraint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactVelocityConstraint.Create: b2ContactVelocityConstraint; cdecl;
begin
  Result := b2ContactVelocityConstraint_Create;
end;


function b2ContactSolverDef_Create: b2ContactSolverDef; cdecl; external LIB_NAME name _PU + 'b2ContactSolverDef_b2ContactSolverDef'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactSolverDef.Create: b2ContactSolverDef; cdecl;
begin
  Result := b2ContactSolverDef_Create;
end;


function b2ContactSolver_Create(def: Pb2ContactSolverDef): b2ContactSolverHandle; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_b2ContactSolver_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactSolverWrapper.Create(def: Pb2ContactSolverDef): b2ContactSolverWrapper; cdecl;
begin
  Result.FHandle := b2ContactSolver_Create(def);
end;

procedure b2ContactSolver_Destroy(_self: b2ContactSolverHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolverWrapper.Destroy; cdecl;
begin
  b2ContactSolver_Destroy(FHandle);
end;

class operator b2ContactSolverWrapper.Implicit(handle: b2ContactSolverHandle): b2ContactSolverWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ContactSolverWrapper.Implicit(wrapper: b2ContactSolverWrapper): b2ContactSolverHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2ContactSolver_InitializeVelocityConstraints(_self: b2ContactSolverHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_InitializeVelocityConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolverWrapper.InitializeVelocityConstraints; cdecl;
begin
  b2ContactSolver_InitializeVelocityConstraints(FHandle)
end;

procedure b2ContactSolver_WarmStart(_self: b2ContactSolverHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_WarmStart'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolverWrapper.WarmStart; cdecl;
begin
  b2ContactSolver_WarmStart(FHandle)
end;

procedure b2ContactSolver_SolveVelocityConstraints(_self: b2ContactSolverHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_SolveVelocityConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolverWrapper.SolveVelocityConstraints; cdecl;
begin
  b2ContactSolver_SolveVelocityConstraints(FHandle)
end;

procedure b2ContactSolver_StoreImpulses(_self: b2ContactSolverHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_StoreImpulses'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolverWrapper.StoreImpulses; cdecl;
begin
  b2ContactSolver_StoreImpulses(FHandle)
end;

function b2ContactSolver_SolvePositionConstraints(_self: b2ContactSolverHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_SolvePositionConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.SolvePositionConstraints: Boolean; cdecl;
begin
  Result := b2ContactSolver_SolvePositionConstraints(FHandle)
end;

function b2ContactSolver_SolveTOIPositionConstraints(_self: b2ContactSolverHandle; toiIndexA: Integer; toiIndexB: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_SolveTOIPositionConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.SolveTOIPositionConstraints(toiIndexA: Integer; toiIndexB: Integer): Boolean; cdecl;
begin
  Result := b2ContactSolver_SolveTOIPositionConstraints(FHandle, toiIndexA, toiIndexB)
end;

function b2ContactSolver_Get_m_step(_self: b2ContactSolverHandle): b2TimeStep; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_step'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_step(_self: b2ContactSolverHandle; aNewValue: b2TimeStep); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_step'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_step: b2TimeStep; cdecl;
begin
  Result := b2ContactSolver_Get_m_step(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_step(aNewValue: b2TimeStep); cdecl;
begin
  b2ContactSolver_Set_m_step(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_positions(_self: b2ContactSolverHandle): Pb2Position; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_positions'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_positions(_self: b2ContactSolverHandle; aNewValue: Pb2Position); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_positions'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_positions: Pb2Position; cdecl;
begin
  Result := b2ContactSolver_Get_m_positions(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_positions(aNewValue: Pb2Position); cdecl;
begin
  b2ContactSolver_Set_m_positions(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_velocities(_self: b2ContactSolverHandle): Pb2Velocity; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_velocities'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_velocities(_self: b2ContactSolverHandle; aNewValue: Pb2Velocity); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_velocities'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_velocities: Pb2Velocity; cdecl;
begin
  Result := b2ContactSolver_Get_m_velocities(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_velocities(aNewValue: Pb2Velocity); cdecl;
begin
  b2ContactSolver_Set_m_velocities(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_allocator(_self: b2ContactSolverHandle): b2StackAllocatorHandle; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_allocator(_self: b2ContactSolverHandle; aNewValue: b2StackAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_allocator'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_allocator: b2StackAllocatorHandle; cdecl;
begin
  Result := b2ContactSolver_Get_m_allocator(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_allocator(aNewValue: b2StackAllocatorHandle); cdecl;
begin
  b2ContactSolver_Set_m_allocator(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_velocityConstraints(_self: b2ContactSolverHandle): Pb2ContactVelocityConstraint; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_velocityConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_velocityConstraints(_self: b2ContactSolverHandle; aNewValue: Pb2ContactVelocityConstraint); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_velocityConstraints'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_velocityConstraints: Pb2ContactVelocityConstraint; cdecl;
begin
  Result := b2ContactSolver_Get_m_velocityConstraints(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_velocityConstraints(aNewValue: Pb2ContactVelocityConstraint); cdecl;
begin
  b2ContactSolver_Set_m_velocityConstraints(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_contacts(_self: b2ContactSolverHandle): Pb2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_contacts'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_contacts(_self: b2ContactSolverHandle; aNewValue: Pb2ContactHandle); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_contacts'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_contacts: Pb2ContactHandle; cdecl;
begin
  Result := b2ContactSolver_Get_m_contacts(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_contacts(aNewValue: Pb2ContactHandle); cdecl;
begin
  b2ContactSolver_Set_m_contacts(FHandle, aNewValue);
end;

function b2ContactSolver_Get_m_count(_self: b2ContactSolverHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Get_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ContactSolver_Set_m_count(_self: b2ContactSolverHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2ContactSolver_Set_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ContactSolverWrapper.Get_m_count: Integer; cdecl;
begin
  Result := b2ContactSolver_Get_m_count(FHandle);
end;

procedure b2ContactSolverWrapper.Set_m_count(aNewValue: Integer); cdecl;
begin
  b2ContactSolver_Set_m_count(FHandle, aNewValue);
end;


function b2EdgeAndCircleContact_Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndCircleContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_b2EdgeAndCircleContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2EdgeAndCircleContactWrapper.Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndCircleContactWrapper; cdecl;
begin
  Result.FHandle := b2EdgeAndCircleContact_Create(fixtureA, fixtureB);
end;

procedure b2EdgeAndCircleContact_Destroy(_self: b2EdgeAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.Destroy; cdecl;
begin
  b2EdgeAndCircleContact_Destroy(FHandle);
end;

class operator b2EdgeAndCircleContactWrapper.Implicit(handle: b2EdgeAndCircleContactHandle): b2EdgeAndCircleContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2EdgeAndCircleContactWrapper.Implicit(wrapper: b2EdgeAndCircleContactWrapper): b2EdgeAndCircleContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2EdgeAndCircleContact_GetManifold(_self: b2EdgeAndCircleContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetManifold(FHandle)
end;

procedure b2EdgeAndCircleContact_GetWorldManifold(_self: b2EdgeAndCircleContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2EdgeAndCircleContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2EdgeAndCircleContact_IsTouching(_self: b2EdgeAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2EdgeAndCircleContact_IsTouching(FHandle)
end;

procedure b2EdgeAndCircleContact_SetEnabled(_self: b2EdgeAndCircleContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2EdgeAndCircleContact_SetEnabled(FHandle, flag)
end;

function b2EdgeAndCircleContact_IsEnabled(_self: b2EdgeAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2EdgeAndCircleContact_IsEnabled(FHandle)
end;

function b2EdgeAndCircleContact_GetNext(_self: b2EdgeAndCircleContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetNext(FHandle)
end;

function b2EdgeAndCircleContact_GetFixtureA(_self: b2EdgeAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetFixtureA(FHandle)
end;

function b2EdgeAndCircleContact_GetChildIndexA(_self: b2EdgeAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetChildIndexA(FHandle)
end;

function b2EdgeAndCircleContact_GetFixtureB(_self: b2EdgeAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetFixtureB(FHandle)
end;

function b2EdgeAndCircleContact_GetChildIndexB(_self: b2EdgeAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetChildIndexB(FHandle)
end;

procedure b2EdgeAndCircleContact_SetFriction(_self: b2EdgeAndCircleContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2EdgeAndCircleContact_SetFriction(FHandle, friction)
end;

function b2EdgeAndCircleContact_GetFriction(_self: b2EdgeAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetFriction(FHandle)
end;

procedure b2EdgeAndCircleContact_ResetFriction(_self: b2EdgeAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.ResetFriction; cdecl;
begin
  b2EdgeAndCircleContact_ResetFriction(FHandle)
end;

procedure b2EdgeAndCircleContact_SetRestitution(_self: b2EdgeAndCircleContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2EdgeAndCircleContact_SetRestitution(FHandle, restitution)
end;

function b2EdgeAndCircleContact_GetRestitution(_self: b2EdgeAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetRestitution(FHandle)
end;

procedure b2EdgeAndCircleContact_ResetRestitution(_self: b2EdgeAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.ResetRestitution; cdecl;
begin
  b2EdgeAndCircleContact_ResetRestitution(FHandle)
end;

procedure b2EdgeAndCircleContact_SetTangentSpeed(_self: b2EdgeAndCircleContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2EdgeAndCircleContact_SetTangentSpeed(FHandle, speed)
end;

function b2EdgeAndCircleContact_GetTangentSpeed(_self: b2EdgeAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2EdgeAndCircleContact_GetTangentSpeed(FHandle)
end;

procedure b2EdgeAndCircleContact_Evaluate(_self: b2EdgeAndCircleContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2EdgeAndCircleContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2EdgeAndCircleContact_Create_(_self: b2EdgeAndCircleContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndCircleContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2EdgeAndCircleContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2EdgeAndCircleContact_Destroy_(_self: b2EdgeAndCircleContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndCircleContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndCircleContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2EdgeAndCircleContact_Destroy_(FHandle, contact, allocator)
end;


function b2EdgeAndPolygonContact_Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndPolygonContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_b2EdgeAndPolygonContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2EdgeAndPolygonContactWrapper.Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2EdgeAndPolygonContactWrapper; cdecl;
begin
  Result.FHandle := b2EdgeAndPolygonContact_Create(fixtureA, fixtureB);
end;

procedure b2EdgeAndPolygonContact_Destroy(_self: b2EdgeAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.Destroy; cdecl;
begin
  b2EdgeAndPolygonContact_Destroy(FHandle);
end;

class operator b2EdgeAndPolygonContactWrapper.Implicit(handle: b2EdgeAndPolygonContactHandle): b2EdgeAndPolygonContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2EdgeAndPolygonContactWrapper.Implicit(wrapper: b2EdgeAndPolygonContactWrapper): b2EdgeAndPolygonContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2EdgeAndPolygonContact_GetManifold(_self: b2EdgeAndPolygonContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetManifold(FHandle)
end;

procedure b2EdgeAndPolygonContact_GetWorldManifold(_self: b2EdgeAndPolygonContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2EdgeAndPolygonContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2EdgeAndPolygonContact_IsTouching(_self: b2EdgeAndPolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2EdgeAndPolygonContact_IsTouching(FHandle)
end;

procedure b2EdgeAndPolygonContact_SetEnabled(_self: b2EdgeAndPolygonContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2EdgeAndPolygonContact_SetEnabled(FHandle, flag)
end;

function b2EdgeAndPolygonContact_IsEnabled(_self: b2EdgeAndPolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2EdgeAndPolygonContact_IsEnabled(FHandle)
end;

function b2EdgeAndPolygonContact_GetNext(_self: b2EdgeAndPolygonContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetNext(FHandle)
end;

function b2EdgeAndPolygonContact_GetFixtureA(_self: b2EdgeAndPolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetFixtureA(FHandle)
end;

function b2EdgeAndPolygonContact_GetChildIndexA(_self: b2EdgeAndPolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetChildIndexA(FHandle)
end;

function b2EdgeAndPolygonContact_GetFixtureB(_self: b2EdgeAndPolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetFixtureB(FHandle)
end;

function b2EdgeAndPolygonContact_GetChildIndexB(_self: b2EdgeAndPolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetChildIndexB(FHandle)
end;

procedure b2EdgeAndPolygonContact_SetFriction(_self: b2EdgeAndPolygonContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2EdgeAndPolygonContact_SetFriction(FHandle, friction)
end;

function b2EdgeAndPolygonContact_GetFriction(_self: b2EdgeAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetFriction(FHandle)
end;

procedure b2EdgeAndPolygonContact_ResetFriction(_self: b2EdgeAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.ResetFriction; cdecl;
begin
  b2EdgeAndPolygonContact_ResetFriction(FHandle)
end;

procedure b2EdgeAndPolygonContact_SetRestitution(_self: b2EdgeAndPolygonContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2EdgeAndPolygonContact_SetRestitution(FHandle, restitution)
end;

function b2EdgeAndPolygonContact_GetRestitution(_self: b2EdgeAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetRestitution(FHandle)
end;

procedure b2EdgeAndPolygonContact_ResetRestitution(_self: b2EdgeAndPolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.ResetRestitution; cdecl;
begin
  b2EdgeAndPolygonContact_ResetRestitution(FHandle)
end;

procedure b2EdgeAndPolygonContact_SetTangentSpeed(_self: b2EdgeAndPolygonContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2EdgeAndPolygonContact_SetTangentSpeed(FHandle, speed)
end;

function b2EdgeAndPolygonContact_GetTangentSpeed(_self: b2EdgeAndPolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2EdgeAndPolygonContact_GetTangentSpeed(FHandle)
end;

procedure b2EdgeAndPolygonContact_Evaluate(_self: b2EdgeAndPolygonContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2EdgeAndPolygonContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2EdgeAndPolygonContact_Create_(_self: b2EdgeAndPolygonContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeAndPolygonContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2EdgeAndPolygonContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2EdgeAndPolygonContact_Destroy_(_self: b2EdgeAndPolygonContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeAndPolygonContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeAndPolygonContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2EdgeAndPolygonContact_Destroy_(FHandle, contact, allocator)
end;


function b2PolygonAndCircleContact_Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonAndCircleContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_b2PolygonAndCircleContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2PolygonAndCircleContactWrapper.Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonAndCircleContactWrapper; cdecl;
begin
  Result.FHandle := b2PolygonAndCircleContact_Create(fixtureA, fixtureB);
end;

procedure b2PolygonAndCircleContact_Destroy(_self: b2PolygonAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.Destroy; cdecl;
begin
  b2PolygonAndCircleContact_Destroy(FHandle);
end;

class operator b2PolygonAndCircleContactWrapper.Implicit(handle: b2PolygonAndCircleContactHandle): b2PolygonAndCircleContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2PolygonAndCircleContactWrapper.Implicit(wrapper: b2PolygonAndCircleContactWrapper): b2PolygonAndCircleContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2PolygonAndCircleContact_GetManifold(_self: b2PolygonAndCircleContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetManifold(FHandle)
end;

procedure b2PolygonAndCircleContact_GetWorldManifold(_self: b2PolygonAndCircleContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2PolygonAndCircleContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2PolygonAndCircleContact_IsTouching(_self: b2PolygonAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2PolygonAndCircleContact_IsTouching(FHandle)
end;

procedure b2PolygonAndCircleContact_SetEnabled(_self: b2PolygonAndCircleContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2PolygonAndCircleContact_SetEnabled(FHandle, flag)
end;

function b2PolygonAndCircleContact_IsEnabled(_self: b2PolygonAndCircleContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2PolygonAndCircleContact_IsEnabled(FHandle)
end;

function b2PolygonAndCircleContact_GetNext(_self: b2PolygonAndCircleContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetNext(FHandle)
end;

function b2PolygonAndCircleContact_GetFixtureA(_self: b2PolygonAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetFixtureA(FHandle)
end;

function b2PolygonAndCircleContact_GetChildIndexA(_self: b2PolygonAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetChildIndexA(FHandle)
end;

function b2PolygonAndCircleContact_GetFixtureB(_self: b2PolygonAndCircleContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetFixtureB(FHandle)
end;

function b2PolygonAndCircleContact_GetChildIndexB(_self: b2PolygonAndCircleContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetChildIndexB(FHandle)
end;

procedure b2PolygonAndCircleContact_SetFriction(_self: b2PolygonAndCircleContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2PolygonAndCircleContact_SetFriction(FHandle, friction)
end;

function b2PolygonAndCircleContact_GetFriction(_self: b2PolygonAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetFriction(FHandle)
end;

procedure b2PolygonAndCircleContact_ResetFriction(_self: b2PolygonAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.ResetFriction; cdecl;
begin
  b2PolygonAndCircleContact_ResetFriction(FHandle)
end;

procedure b2PolygonAndCircleContact_SetRestitution(_self: b2PolygonAndCircleContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2PolygonAndCircleContact_SetRestitution(FHandle, restitution)
end;

function b2PolygonAndCircleContact_GetRestitution(_self: b2PolygonAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetRestitution(FHandle)
end;

procedure b2PolygonAndCircleContact_ResetRestitution(_self: b2PolygonAndCircleContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.ResetRestitution; cdecl;
begin
  b2PolygonAndCircleContact_ResetRestitution(FHandle)
end;

procedure b2PolygonAndCircleContact_SetTangentSpeed(_self: b2PolygonAndCircleContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2PolygonAndCircleContact_SetTangentSpeed(FHandle, speed)
end;

function b2PolygonAndCircleContact_GetTangentSpeed(_self: b2PolygonAndCircleContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2PolygonAndCircleContact_GetTangentSpeed(FHandle)
end;

procedure b2PolygonAndCircleContact_Evaluate(_self: b2PolygonAndCircleContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2PolygonAndCircleContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2PolygonAndCircleContact_Create_(_self: b2PolygonAndCircleContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonAndCircleContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2PolygonAndCircleContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2PolygonAndCircleContact_Destroy_(_self: b2PolygonAndCircleContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonAndCircleContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonAndCircleContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2PolygonAndCircleContact_Destroy_(FHandle, contact, allocator)
end;


function b2PolygonContact_Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_b2PolygonContact_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2PolygonContactWrapper.Create(fixtureA: Pb2Fixture; fixtureB: Pb2Fixture): b2PolygonContactWrapper; cdecl;
begin
  Result.FHandle := b2PolygonContact_Create(fixtureA, fixtureB);
end;

procedure b2PolygonContact_Destroy(_self: b2PolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.Destroy; cdecl;
begin
  b2PolygonContact_Destroy(FHandle);
end;

class operator b2PolygonContactWrapper.Implicit(handle: b2PolygonContactHandle): b2PolygonContactWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2PolygonContactWrapper.Implicit(wrapper: b2PolygonContactWrapper): b2PolygonContactHandle;
begin
  Result := wrapper.FHandle;
end;

function b2PolygonContact_GetManifold(_self: b2PolygonContactHandle): Pb2Manifold; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetManifold: Pb2Manifold; cdecl;
begin
  Result := b2PolygonContact_GetManifold(FHandle)
end;

procedure b2PolygonContact_GetWorldManifold(_self: b2PolygonContactHandle; worldManifold: Pb2WorldManifold); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetWorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.GetWorldManifold(worldManifold: Pb2WorldManifold); cdecl;
begin
  b2PolygonContact_GetWorldManifold(FHandle, worldManifold)
end;

function b2PolygonContact_IsTouching(_self: b2PolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_IsTouching'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.IsTouching: Boolean; cdecl;
begin
  Result := b2PolygonContact_IsTouching(FHandle)
end;

procedure b2PolygonContact_SetEnabled(_self: b2PolygonContactHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_SetEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.SetEnabled(flag: Boolean); cdecl;
begin
  b2PolygonContact_SetEnabled(FHandle, flag)
end;

function b2PolygonContact_IsEnabled(_self: b2PolygonContactHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_IsEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.IsEnabled: Boolean; cdecl;
begin
  Result := b2PolygonContact_IsEnabled(FHandle)
end;

function b2PolygonContact_GetNext(_self: b2PolygonContactHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetNext: b2ContactHandle; cdecl;
begin
  Result := b2PolygonContact_GetNext(FHandle)
end;

function b2PolygonContact_GetFixtureA(_self: b2PolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetFixtureA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetFixtureA: Pb2Fixture; cdecl;
begin
  Result := b2PolygonContact_GetFixtureA(FHandle)
end;

function b2PolygonContact_GetChildIndexA(_self: b2PolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetChildIndexA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetChildIndexA: Integer; cdecl;
begin
  Result := b2PolygonContact_GetChildIndexA(FHandle)
end;

function b2PolygonContact_GetFixtureB(_self: b2PolygonContactHandle): Pb2Fixture; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetFixtureB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetFixtureB: Pb2Fixture; cdecl;
begin
  Result := b2PolygonContact_GetFixtureB(FHandle)
end;

function b2PolygonContact_GetChildIndexB(_self: b2PolygonContactHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetChildIndexB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetChildIndexB: Integer; cdecl;
begin
  Result := b2PolygonContact_GetChildIndexB(FHandle)
end;

procedure b2PolygonContact_SetFriction(_self: b2PolygonContactHandle; friction: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_SetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.SetFriction(friction: Single); cdecl;
begin
  b2PolygonContact_SetFriction(FHandle, friction)
end;

function b2PolygonContact_GetFriction(_self: b2PolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetFriction: Single; cdecl;
begin
  Result := b2PolygonContact_GetFriction(FHandle)
end;

procedure b2PolygonContact_ResetFriction(_self: b2PolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_ResetFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.ResetFriction; cdecl;
begin
  b2PolygonContact_ResetFriction(FHandle)
end;

procedure b2PolygonContact_SetRestitution(_self: b2PolygonContactHandle; restitution: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_SetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.SetRestitution(restitution: Single); cdecl;
begin
  b2PolygonContact_SetRestitution(FHandle, restitution)
end;

function b2PolygonContact_GetRestitution(_self: b2PolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetRestitution: Single; cdecl;
begin
  Result := b2PolygonContact_GetRestitution(FHandle)
end;

procedure b2PolygonContact_ResetRestitution(_self: b2PolygonContactHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_ResetRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.ResetRestitution; cdecl;
begin
  b2PolygonContact_ResetRestitution(FHandle)
end;

procedure b2PolygonContact_SetTangentSpeed(_self: b2PolygonContactHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_SetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.SetTangentSpeed(speed: Single); cdecl;
begin
  b2PolygonContact_SetTangentSpeed(FHandle, speed)
end;

function b2PolygonContact_GetTangentSpeed(_self: b2PolygonContactHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_GetTangentSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.GetTangentSpeed: Single; cdecl;
begin
  Result := b2PolygonContact_GetTangentSpeed(FHandle)
end;

procedure b2PolygonContact_Evaluate(_self: b2PolygonContactHandle; manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_Evaluate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.Evaluate(manifold: Pb2Manifold; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform); cdecl;
begin
  b2PolygonContact_Evaluate(FHandle, manifold, xfA, xfB)
end;

function b2PolygonContact_Create_(_self: b2PolygonContactHandle; fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonContact_Create'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonContactWrapper.Create_(fixtureA: Pb2Fixture; indexA: Integer; fixtureB: Pb2Fixture; indexB: Integer; allocator: b2BlockAllocatorHandle): b2ContactHandle; cdecl;
begin
  Result := b2PolygonContact_Create_(FHandle, fixtureA, indexA, fixtureB, indexB, allocator)
end;

procedure b2PolygonContact_Destroy_(_self: b2PolygonContactHandle; contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonContact_Destroy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonContactWrapper.Destroy_(contact: b2ContactHandle; allocator: b2BlockAllocatorHandle); cdecl;
begin
  b2PolygonContact_Destroy_(FHandle, contact, allocator)
end;


function b2Jacobian_Create: b2Jacobian; cdecl; external LIB_NAME name _PU + 'b2Jacobian_b2Jacobian'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Jacobian.Create: b2Jacobian; cdecl;
begin
  Result := b2Jacobian_Create;
end;


function b2JointEdge_Create: b2JointEdge; cdecl; external LIB_NAME name _PU + 'b2JointEdge_b2JointEdge'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2JointEdge.Create: b2JointEdge; cdecl;
begin
  Result := b2JointEdge_Create;
end;


function b2JointDef_Create: b2JointDef; cdecl; external LIB_NAME name _PU + 'b2JointDef_b2JointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2JointDef.Create: b2JointDef; cdecl;
begin
  Result := b2JointDef_Create;
end;


class operator b2JointWrapper.Implicit(handle: b2JointHandle): b2JointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2JointWrapper.Implicit(wrapper: b2JointWrapper): b2JointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2Joint_GetType(_self: b2JointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2Joint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2Joint_GetType(FHandle)
end;

function b2Joint_GetBodyA(_self: b2JointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2Joint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2Joint_GetBodyA(FHandle)
end;

function b2Joint_GetBodyB(_self: b2JointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2Joint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2Joint_GetBodyB(FHandle)
end;

function b2Joint_GetAnchorA(_self: b2JointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Joint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2Joint_GetAnchorA(FHandle)
end;

function b2Joint_GetAnchorB(_self: b2JointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Joint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2Joint_GetAnchorB(FHandle)
end;

function b2Joint_GetReactionForce(_self: b2JointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Joint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2Joint_GetReactionForce(FHandle, inv_dt)
end;

function b2Joint_GetReactionTorque(_self: b2JointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2Joint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2Joint_GetReactionTorque(FHandle, inv_dt)
end;

function b2Joint_GetNext(_self: b2JointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2Joint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2Joint_GetNext(FHandle)
end;

function b2Joint_GetUserData(_self: b2JointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2Joint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2Joint_GetUserData(FHandle)
end;

procedure b2Joint_SetUserData(_self: b2JointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2Joint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2JointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2Joint_SetUserData(FHandle, data)
end;

function b2Joint_IsActive(_self: b2JointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Joint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2Joint_IsActive(FHandle)
end;

function b2Joint_GetCollideConnected(_self: b2JointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2Joint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2JointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2Joint_GetCollideConnected(FHandle)
end;

procedure b2Joint_Dump(_self: b2JointHandle); cdecl; external LIB_NAME name _PU + 'b2Joint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2JointWrapper.Dump; cdecl;
begin
  b2Joint_Dump(FHandle)
end;

procedure b2Joint_ShiftOrigin(_self: b2JointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2Joint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2JointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2Joint_ShiftOrigin(FHandle, newOrigin)
end;


function b2DistanceJointDef_Create: b2DistanceJointDef; cdecl; external LIB_NAME name _PU + 'b2DistanceJointDef_b2DistanceJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2DistanceJointDef.Create: b2DistanceJointDef; cdecl;
begin
  Result := b2DistanceJointDef_Create;
end;

procedure b2DistanceJointDef_Initialize(_self: Pb2DistanceJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2DistanceJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2); cdecl;
begin
  b2DistanceJointDef_Initialize(@Self, bodyA, bodyB, anchorA, anchorB)
end;


procedure b2DistanceJoint_Destroy(_self: b2DistanceJointHandle); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.Destroy; cdecl;
begin
  b2DistanceJoint_Destroy(FHandle);
end;

class operator b2DistanceJointWrapper.Implicit(handle: b2DistanceJointHandle): b2DistanceJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2DistanceJointWrapper.Implicit(wrapper: b2DistanceJointWrapper): b2DistanceJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2DistanceJoint_GetType(_self: b2DistanceJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2DistanceJoint_GetType(FHandle)
end;

function b2DistanceJoint_GetBodyA(_self: b2DistanceJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2DistanceJoint_GetBodyA(FHandle)
end;

function b2DistanceJoint_GetBodyB(_self: b2DistanceJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2DistanceJoint_GetBodyB(FHandle)
end;

function b2DistanceJoint_GetAnchorA(_self: b2DistanceJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2DistanceJoint_GetAnchorA(FHandle)
end;

function b2DistanceJoint_GetAnchorB(_self: b2DistanceJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2DistanceJoint_GetAnchorB(FHandle)
end;

function b2DistanceJoint_GetReactionForce(_self: b2DistanceJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2DistanceJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2DistanceJoint_GetReactionTorque(_self: b2DistanceJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2DistanceJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2DistanceJoint_GetNext(_self: b2DistanceJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2DistanceJoint_GetNext(FHandle)
end;

function b2DistanceJoint_GetUserData(_self: b2DistanceJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2DistanceJoint_GetUserData(FHandle)
end;

procedure b2DistanceJoint_SetUserData(_self: b2DistanceJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2DistanceJoint_SetUserData(FHandle, data)
end;

function b2DistanceJoint_IsActive(_self: b2DistanceJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2DistanceJoint_IsActive(FHandle)
end;

function b2DistanceJoint_GetCollideConnected(_self: b2DistanceJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2DistanceJoint_GetCollideConnected(FHandle)
end;

procedure b2DistanceJoint_Dump(_self: b2DistanceJointHandle); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.Dump; cdecl;
begin
  b2DistanceJoint_Dump(FHandle)
end;

procedure b2DistanceJoint_ShiftOrigin(_self: b2DistanceJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2DistanceJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2DistanceJoint_GetLocalAnchorA(_self: b2DistanceJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2DistanceJoint_GetLocalAnchorA(FHandle)
end;

function b2DistanceJoint_GetLocalAnchorB(_self: b2DistanceJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2DistanceJoint_GetLocalAnchorB(FHandle)
end;

procedure b2DistanceJoint_SetLength(_self: b2DistanceJointHandle; length: Single); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_SetLength'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.SetLength(length: Single); cdecl;
begin
  b2DistanceJoint_SetLength(FHandle, length)
end;

function b2DistanceJoint_GetLength(_self: b2DistanceJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetLength'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetLength: Single; cdecl;
begin
  Result := b2DistanceJoint_GetLength(FHandle)
end;

procedure b2DistanceJoint_SetFrequency(_self: b2DistanceJointHandle; hz: Single); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_SetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.SetFrequency(hz: Single); cdecl;
begin
  b2DistanceJoint_SetFrequency(FHandle, hz)
end;

function b2DistanceJoint_GetFrequency(_self: b2DistanceJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetFrequency: Single; cdecl;
begin
  Result := b2DistanceJoint_GetFrequency(FHandle)
end;

procedure b2DistanceJoint_SetDampingRatio(_self: b2DistanceJointHandle; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_SetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceJointWrapper.SetDampingRatio(ratio: Single); cdecl;
begin
  b2DistanceJoint_SetDampingRatio(FHandle, ratio)
end;

function b2DistanceJoint_GetDampingRatio(_self: b2DistanceJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2DistanceJoint_GetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceJointWrapper.GetDampingRatio: Single; cdecl;
begin
  Result := b2DistanceJoint_GetDampingRatio(FHandle)
end;


function b2FrictionJointDef_Create: b2FrictionJointDef; cdecl; external LIB_NAME name _PU + 'b2FrictionJointDef_b2FrictionJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2FrictionJointDef.Create: b2FrictionJointDef; cdecl;
begin
  Result := b2FrictionJointDef_Create;
end;

procedure b2FrictionJointDef_Initialize(_self: Pb2FrictionJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2FrictionJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
begin
  b2FrictionJointDef_Initialize(@Self, bodyA, bodyB, anchor)
end;


procedure b2FrictionJoint_Destroy(_self: b2FrictionJointHandle); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.Destroy; cdecl;
begin
  b2FrictionJoint_Destroy(FHandle);
end;

class operator b2FrictionJointWrapper.Implicit(handle: b2FrictionJointHandle): b2FrictionJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2FrictionJointWrapper.Implicit(wrapper: b2FrictionJointWrapper): b2FrictionJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2FrictionJoint_GetType(_self: b2FrictionJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2FrictionJoint_GetType(FHandle)
end;

function b2FrictionJoint_GetBodyA(_self: b2FrictionJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2FrictionJoint_GetBodyA(FHandle)
end;

function b2FrictionJoint_GetBodyB(_self: b2FrictionJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2FrictionJoint_GetBodyB(FHandle)
end;

function b2FrictionJoint_GetAnchorA(_self: b2FrictionJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2FrictionJoint_GetAnchorA(FHandle)
end;

function b2FrictionJoint_GetAnchorB(_self: b2FrictionJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2FrictionJoint_GetAnchorB(FHandle)
end;

function b2FrictionJoint_GetReactionForce(_self: b2FrictionJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2FrictionJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2FrictionJoint_GetReactionTorque(_self: b2FrictionJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2FrictionJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2FrictionJoint_GetNext(_self: b2FrictionJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2FrictionJoint_GetNext(FHandle)
end;

function b2FrictionJoint_GetUserData(_self: b2FrictionJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2FrictionJoint_GetUserData(FHandle)
end;

procedure b2FrictionJoint_SetUserData(_self: b2FrictionJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2FrictionJoint_SetUserData(FHandle, data)
end;

function b2FrictionJoint_IsActive(_self: b2FrictionJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2FrictionJoint_IsActive(FHandle)
end;

function b2FrictionJoint_GetCollideConnected(_self: b2FrictionJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2FrictionJoint_GetCollideConnected(FHandle)
end;

procedure b2FrictionJoint_Dump(_self: b2FrictionJointHandle); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.Dump; cdecl;
begin
  b2FrictionJoint_Dump(FHandle)
end;

procedure b2FrictionJoint_ShiftOrigin(_self: b2FrictionJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2FrictionJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2FrictionJoint_GetLocalAnchorA(_self: b2FrictionJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2FrictionJoint_GetLocalAnchorA(FHandle)
end;

function b2FrictionJoint_GetLocalAnchorB(_self: b2FrictionJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2FrictionJoint_GetLocalAnchorB(FHandle)
end;

procedure b2FrictionJoint_SetMaxForce(_self: b2FrictionJointHandle; force: Single); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_SetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.SetMaxForce(force: Single); cdecl;
begin
  b2FrictionJoint_SetMaxForce(FHandle, force)
end;

function b2FrictionJoint_GetMaxForce(_self: b2FrictionJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetMaxForce: Single; cdecl;
begin
  Result := b2FrictionJoint_GetMaxForce(FHandle)
end;

procedure b2FrictionJoint_SetMaxTorque(_self: b2FrictionJointHandle; torque: Single); cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_SetMaxTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2FrictionJointWrapper.SetMaxTorque(torque: Single); cdecl;
begin
  b2FrictionJoint_SetMaxTorque(FHandle, torque)
end;

function b2FrictionJoint_GetMaxTorque(_self: b2FrictionJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2FrictionJoint_GetMaxTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2FrictionJointWrapper.GetMaxTorque: Single; cdecl;
begin
  Result := b2FrictionJoint_GetMaxTorque(FHandle)
end;


function b2GearJointDef_Create: b2GearJointDef; cdecl; external LIB_NAME name _PU + 'b2GearJointDef_b2GearJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2GearJointDef.Create: b2GearJointDef; cdecl;
begin
  Result := b2GearJointDef_Create;
end;


procedure b2GearJoint_Destroy(_self: b2GearJointHandle); cdecl; external LIB_NAME name _PU + 'b2GearJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2GearJointWrapper.Destroy; cdecl;
begin
  b2GearJoint_Destroy(FHandle);
end;

class operator b2GearJointWrapper.Implicit(handle: b2GearJointHandle): b2GearJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2GearJointWrapper.Implicit(wrapper: b2GearJointWrapper): b2GearJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2GearJoint_GetType(_self: b2GearJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2GearJoint_GetType(FHandle)
end;

function b2GearJoint_GetBodyA(_self: b2GearJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2GearJoint_GetBodyA(FHandle)
end;

function b2GearJoint_GetBodyB(_self: b2GearJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2GearJoint_GetBodyB(FHandle)
end;

function b2GearJoint_GetAnchorA(_self: b2GearJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2GearJoint_GetAnchorA(FHandle)
end;

function b2GearJoint_GetAnchorB(_self: b2GearJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2GearJoint_GetAnchorB(FHandle)
end;

function b2GearJoint_GetReactionForce(_self: b2GearJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2GearJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2GearJoint_GetReactionTorque(_self: b2GearJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2GearJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2GearJoint_GetNext(_self: b2GearJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2GearJoint_GetNext(FHandle)
end;

function b2GearJoint_GetUserData(_self: b2GearJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2GearJoint_GetUserData(FHandle)
end;

procedure b2GearJoint_SetUserData(_self: b2GearJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2GearJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2GearJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2GearJoint_SetUserData(FHandle, data)
end;

function b2GearJoint_IsActive(_self: b2GearJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2GearJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2GearJoint_IsActive(FHandle)
end;

function b2GearJoint_GetCollideConnected(_self: b2GearJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2GearJoint_GetCollideConnected(FHandle)
end;

procedure b2GearJoint_Dump(_self: b2GearJointHandle); cdecl; external LIB_NAME name _PU + 'b2GearJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2GearJointWrapper.Dump; cdecl;
begin
  b2GearJoint_Dump(FHandle)
end;

procedure b2GearJoint_ShiftOrigin(_self: b2GearJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2GearJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2GearJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2GearJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2GearJoint_GetJoint1(_self: b2GearJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetJoint1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetJoint1: b2JointHandle; cdecl;
begin
  Result := b2GearJoint_GetJoint1(FHandle)
end;

function b2GearJoint_GetJoint2(_self: b2GearJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetJoint2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetJoint2: b2JointHandle; cdecl;
begin
  Result := b2GearJoint_GetJoint2(FHandle)
end;

procedure b2GearJoint_SetRatio(_self: b2GearJointHandle; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2GearJoint_SetRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2GearJointWrapper.SetRatio(ratio: Single); cdecl;
begin
  b2GearJoint_SetRatio(FHandle, ratio)
end;

function b2GearJoint_GetRatio(_self: b2GearJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2GearJoint_GetRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2GearJointWrapper.GetRatio: Single; cdecl;
begin
  Result := b2GearJoint_GetRatio(FHandle)
end;


function b2MotorJointDef_Create: b2MotorJointDef; cdecl; external LIB_NAME name _PU + 'b2MotorJointDef_b2MotorJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2MotorJointDef.Create: b2MotorJointDef; cdecl;
begin
  Result := b2MotorJointDef_Create;
end;

procedure b2MotorJointDef_Initialize(_self: Pb2MotorJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle); cdecl; external LIB_NAME name _PU + 'b2MotorJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle); cdecl;
begin
  b2MotorJointDef_Initialize(@Self, bodyA, bodyB)
end;


procedure b2MotorJoint_Destroy(_self: b2MotorJointHandle); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.Destroy; cdecl;
begin
  b2MotorJoint_Destroy(FHandle);
end;

class operator b2MotorJointWrapper.Implicit(handle: b2MotorJointHandle): b2MotorJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2MotorJointWrapper.Implicit(wrapper: b2MotorJointWrapper): b2MotorJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2MotorJoint_GetType(_self: b2MotorJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2MotorJoint_GetType(FHandle)
end;

function b2MotorJoint_GetBodyA(_self: b2MotorJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2MotorJoint_GetBodyA(FHandle)
end;

function b2MotorJoint_GetBodyB(_self: b2MotorJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2MotorJoint_GetBodyB(FHandle)
end;

function b2MotorJoint_GetAnchorA(_self: b2MotorJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2MotorJoint_GetAnchorA(FHandle)
end;

function b2MotorJoint_GetAnchorB(_self: b2MotorJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2MotorJoint_GetAnchorB(FHandle)
end;

function b2MotorJoint_GetReactionForce(_self: b2MotorJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2MotorJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2MotorJoint_GetReactionTorque(_self: b2MotorJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2MotorJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2MotorJoint_GetNext(_self: b2MotorJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2MotorJoint_GetNext(FHandle)
end;

function b2MotorJoint_GetUserData(_self: b2MotorJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2MotorJoint_GetUserData(FHandle)
end;

procedure b2MotorJoint_SetUserData(_self: b2MotorJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2MotorJoint_SetUserData(FHandle, data)
end;

function b2MotorJoint_IsActive(_self: b2MotorJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2MotorJoint_IsActive(FHandle)
end;

function b2MotorJoint_GetCollideConnected(_self: b2MotorJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2MotorJoint_GetCollideConnected(FHandle)
end;

procedure b2MotorJoint_Dump(_self: b2MotorJointHandle); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.Dump; cdecl;
begin
  b2MotorJoint_Dump(FHandle)
end;

procedure b2MotorJoint_ShiftOrigin(_self: b2MotorJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2MotorJoint_ShiftOrigin(FHandle, newOrigin)
end;

procedure b2MotorJoint_SetLinearOffset(_self: b2MotorJointHandle; const [ref] linearOffset: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetLinearOffset'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetLinearOffset(const [ref] linearOffset: b2Vec2); cdecl;
begin
  b2MotorJoint_SetLinearOffset(FHandle, linearOffset)
end;

function b2MotorJoint_GetLinearOffset(_self: b2MotorJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetLinearOffset'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetLinearOffset: Pb2Vec2; cdecl;
begin
  Result := b2MotorJoint_GetLinearOffset(FHandle)
end;

procedure b2MotorJoint_SetAngularOffset(_self: b2MotorJointHandle; angularOffset: Single); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetAngularOffset'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetAngularOffset(angularOffset: Single); cdecl;
begin
  b2MotorJoint_SetAngularOffset(FHandle, angularOffset)
end;

function b2MotorJoint_GetAngularOffset(_self: b2MotorJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetAngularOffset'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetAngularOffset: Single; cdecl;
begin
  Result := b2MotorJoint_GetAngularOffset(FHandle)
end;

procedure b2MotorJoint_SetMaxForce(_self: b2MotorJointHandle; force: Single); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetMaxForce(force: Single); cdecl;
begin
  b2MotorJoint_SetMaxForce(FHandle, force)
end;

function b2MotorJoint_GetMaxForce(_self: b2MotorJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetMaxForce: Single; cdecl;
begin
  Result := b2MotorJoint_GetMaxForce(FHandle)
end;

procedure b2MotorJoint_SetMaxTorque(_self: b2MotorJointHandle; torque: Single); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetMaxTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetMaxTorque(torque: Single); cdecl;
begin
  b2MotorJoint_SetMaxTorque(FHandle, torque)
end;

function b2MotorJoint_GetMaxTorque(_self: b2MotorJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetMaxTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetMaxTorque: Single; cdecl;
begin
  Result := b2MotorJoint_GetMaxTorque(FHandle)
end;

procedure b2MotorJoint_SetCorrectionFactor(_self: b2MotorJointHandle; factor: Single); cdecl; external LIB_NAME name _PU + 'b2MotorJoint_SetCorrectionFactor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MotorJointWrapper.SetCorrectionFactor(factor: Single); cdecl;
begin
  b2MotorJoint_SetCorrectionFactor(FHandle, factor)
end;

function b2MotorJoint_GetCorrectionFactor(_self: b2MotorJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MotorJoint_GetCorrectionFactor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MotorJointWrapper.GetCorrectionFactor: Single; cdecl;
begin
  Result := b2MotorJoint_GetCorrectionFactor(FHandle)
end;


function b2MouseJointDef_Create: b2MouseJointDef; cdecl; external LIB_NAME name _PU + 'b2MouseJointDef_b2MouseJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2MouseJointDef.Create: b2MouseJointDef; cdecl;
begin
  Result := b2MouseJointDef_Create;
end;


procedure b2MouseJoint_Destroy(_self: b2MouseJointHandle); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.Destroy; cdecl;
begin
  b2MouseJoint_Destroy(FHandle);
end;

class operator b2MouseJointWrapper.Implicit(handle: b2MouseJointHandle): b2MouseJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2MouseJointWrapper.Implicit(wrapper: b2MouseJointWrapper): b2MouseJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2MouseJoint_GetType(_self: b2MouseJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2MouseJoint_GetType(FHandle)
end;

function b2MouseJoint_GetBodyA(_self: b2MouseJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2MouseJoint_GetBodyA(FHandle)
end;

function b2MouseJoint_GetBodyB(_self: b2MouseJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2MouseJoint_GetBodyB(FHandle)
end;

function b2MouseJoint_GetAnchorA(_self: b2MouseJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2MouseJoint_GetAnchorA(FHandle)
end;

function b2MouseJoint_GetAnchorB(_self: b2MouseJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2MouseJoint_GetAnchorB(FHandle)
end;

function b2MouseJoint_GetReactionForce(_self: b2MouseJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2MouseJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2MouseJoint_GetReactionTorque(_self: b2MouseJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2MouseJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2MouseJoint_GetNext(_self: b2MouseJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2MouseJoint_GetNext(FHandle)
end;

function b2MouseJoint_GetUserData(_self: b2MouseJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2MouseJoint_GetUserData(FHandle)
end;

procedure b2MouseJoint_SetUserData(_self: b2MouseJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2MouseJoint_SetUserData(FHandle, data)
end;

function b2MouseJoint_IsActive(_self: b2MouseJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2MouseJoint_IsActive(FHandle)
end;

function b2MouseJoint_GetCollideConnected(_self: b2MouseJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2MouseJoint_GetCollideConnected(FHandle)
end;

procedure b2MouseJoint_Dump(_self: b2MouseJointHandle); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.Dump; cdecl;
begin
  b2MouseJoint_Dump(FHandle)
end;

procedure b2MouseJoint_ShiftOrigin(_self: b2MouseJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2MouseJoint_ShiftOrigin(FHandle, newOrigin)
end;

procedure b2MouseJoint_SetTarget(_self: b2MouseJointHandle; const [ref] target: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_SetTarget'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.SetTarget(const [ref] target: b2Vec2); cdecl;
begin
  b2MouseJoint_SetTarget(FHandle, target)
end;

function b2MouseJoint_GetTarget(_self: b2MouseJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetTarget'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetTarget: Pb2Vec2; cdecl;
begin
  Result := b2MouseJoint_GetTarget(FHandle)
end;

procedure b2MouseJoint_SetMaxForce(_self: b2MouseJointHandle; force: Single); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_SetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.SetMaxForce(force: Single); cdecl;
begin
  b2MouseJoint_SetMaxForce(FHandle, force)
end;

function b2MouseJoint_GetMaxForce(_self: b2MouseJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetMaxForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetMaxForce: Single; cdecl;
begin
  Result := b2MouseJoint_GetMaxForce(FHandle)
end;

procedure b2MouseJoint_SetFrequency(_self: b2MouseJointHandle; hz: Single); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_SetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.SetFrequency(hz: Single); cdecl;
begin
  b2MouseJoint_SetFrequency(FHandle, hz)
end;

function b2MouseJoint_GetFrequency(_self: b2MouseJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetFrequency: Single; cdecl;
begin
  Result := b2MouseJoint_GetFrequency(FHandle)
end;

procedure b2MouseJoint_SetDampingRatio(_self: b2MouseJointHandle; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2MouseJoint_SetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2MouseJointWrapper.SetDampingRatio(ratio: Single); cdecl;
begin
  b2MouseJoint_SetDampingRatio(FHandle, ratio)
end;

function b2MouseJoint_GetDampingRatio(_self: b2MouseJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2MouseJoint_GetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MouseJointWrapper.GetDampingRatio: Single; cdecl;
begin
  Result := b2MouseJoint_GetDampingRatio(FHandle)
end;


function b2PrismaticJointDef_Create: b2PrismaticJointDef; cdecl; external LIB_NAME name _PU + 'b2PrismaticJointDef_b2PrismaticJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2PrismaticJointDef.Create: b2PrismaticJointDef; cdecl;
begin
  Result := b2PrismaticJointDef_Create;
end;

procedure b2PrismaticJointDef_Initialize(_self: Pb2PrismaticJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2PrismaticJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl;
begin
  b2PrismaticJointDef_Initialize(@Self, bodyA, bodyB, anchor, axis)
end;


procedure b2PrismaticJoint_Destroy(_self: b2PrismaticJointHandle); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.Destroy; cdecl;
begin
  b2PrismaticJoint_Destroy(FHandle);
end;

class operator b2PrismaticJointWrapper.Implicit(handle: b2PrismaticJointHandle): b2PrismaticJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2PrismaticJointWrapper.Implicit(wrapper: b2PrismaticJointWrapper): b2PrismaticJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2PrismaticJoint_GetType(_self: b2PrismaticJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2PrismaticJoint_GetType(FHandle)
end;

function b2PrismaticJoint_GetBodyA(_self: b2PrismaticJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2PrismaticJoint_GetBodyA(FHandle)
end;

function b2PrismaticJoint_GetBodyB(_self: b2PrismaticJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2PrismaticJoint_GetBodyB(FHandle)
end;

function b2PrismaticJoint_GetAnchorA(_self: b2PrismaticJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetAnchorA(FHandle)
end;

function b2PrismaticJoint_GetAnchorB(_self: b2PrismaticJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetAnchorB(FHandle)
end;

function b2PrismaticJoint_GetReactionForce(_self: b2PrismaticJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2PrismaticJoint_GetReactionTorque(_self: b2PrismaticJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2PrismaticJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2PrismaticJoint_GetNext(_self: b2PrismaticJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2PrismaticJoint_GetNext(FHandle)
end;

function b2PrismaticJoint_GetUserData(_self: b2PrismaticJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2PrismaticJoint_GetUserData(FHandle)
end;

procedure b2PrismaticJoint_SetUserData(_self: b2PrismaticJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2PrismaticJoint_SetUserData(FHandle, data)
end;

function b2PrismaticJoint_IsActive(_self: b2PrismaticJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2PrismaticJoint_IsActive(FHandle)
end;

function b2PrismaticJoint_GetCollideConnected(_self: b2PrismaticJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2PrismaticJoint_GetCollideConnected(FHandle)
end;

procedure b2PrismaticJoint_Dump(_self: b2PrismaticJointHandle); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.Dump; cdecl;
begin
  b2PrismaticJoint_Dump(FHandle)
end;

procedure b2PrismaticJoint_ShiftOrigin(_self: b2PrismaticJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2PrismaticJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2PrismaticJoint_GetLocalAnchorA(_self: b2PrismaticJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetLocalAnchorA(FHandle)
end;

function b2PrismaticJoint_GetLocalAnchorB(_self: b2PrismaticJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetLocalAnchorB(FHandle)
end;

function b2PrismaticJoint_GetLocalAxisA(_self: b2PrismaticJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetLocalAxisA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetLocalAxisA: Pb2Vec2; cdecl;
begin
  Result := b2PrismaticJoint_GetLocalAxisA(FHandle)
end;

function b2PrismaticJoint_GetReferenceAngle(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetReferenceAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetReferenceAngle: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetReferenceAngle(FHandle)
end;

function b2PrismaticJoint_GetJointTranslation(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetJointTranslation'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetJointTranslation: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetJointTranslation(FHandle)
end;

function b2PrismaticJoint_GetJointSpeed(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetJointSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetJointSpeed: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetJointSpeed(FHandle)
end;

function b2PrismaticJoint_IsLimitEnabled(_self: b2PrismaticJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_IsLimitEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.IsLimitEnabled: Boolean; cdecl;
begin
  Result := b2PrismaticJoint_IsLimitEnabled(FHandle)
end;

procedure b2PrismaticJoint_EnableLimit(_self: b2PrismaticJointHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_EnableLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.EnableLimit(flag: Boolean); cdecl;
begin
  b2PrismaticJoint_EnableLimit(FHandle, flag)
end;

function b2PrismaticJoint_GetLowerLimit(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetLowerLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetLowerLimit: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetLowerLimit(FHandle)
end;

function b2PrismaticJoint_GetUpperLimit(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetUpperLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetUpperLimit: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetUpperLimit(FHandle)
end;

procedure b2PrismaticJoint_SetLimits(_self: b2PrismaticJointHandle; lower: Single; upper: Single); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_SetLimits'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.SetLimits(lower: Single; upper: Single); cdecl;
begin
  b2PrismaticJoint_SetLimits(FHandle, lower, upper)
end;

function b2PrismaticJoint_IsMotorEnabled(_self: b2PrismaticJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_IsMotorEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.IsMotorEnabled: Boolean; cdecl;
begin
  Result := b2PrismaticJoint_IsMotorEnabled(FHandle)
end;

procedure b2PrismaticJoint_EnableMotor(_self: b2PrismaticJointHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_EnableMotor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.EnableMotor(flag: Boolean); cdecl;
begin
  b2PrismaticJoint_EnableMotor(FHandle, flag)
end;

procedure b2PrismaticJoint_SetMotorSpeed(_self: b2PrismaticJointHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_SetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.SetMotorSpeed(speed: Single); cdecl;
begin
  b2PrismaticJoint_SetMotorSpeed(FHandle, speed)
end;

function b2PrismaticJoint_GetMotorSpeed(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetMotorSpeed: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetMotorSpeed(FHandle)
end;

procedure b2PrismaticJoint_SetMaxMotorForce(_self: b2PrismaticJointHandle; force: Single); cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_SetMaxMotorForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PrismaticJointWrapper.SetMaxMotorForce(force: Single); cdecl;
begin
  b2PrismaticJoint_SetMaxMotorForce(FHandle, force)
end;

function b2PrismaticJoint_GetMaxMotorForce(_self: b2PrismaticJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetMaxMotorForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetMaxMotorForce: Single; cdecl;
begin
  Result := b2PrismaticJoint_GetMaxMotorForce(FHandle)
end;

function b2PrismaticJoint_GetMotorForce(_self: b2PrismaticJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2PrismaticJoint_GetMotorForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PrismaticJointWrapper.GetMotorForce(inv_dt: Single): Single; cdecl;
begin
  Result := b2PrismaticJoint_GetMotorForce(FHandle, inv_dt)
end;


function b2PulleyJointDef_Create: b2PulleyJointDef; cdecl; external LIB_NAME name _PU + 'b2PulleyJointDef_b2PulleyJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2PulleyJointDef.Create: b2PulleyJointDef; cdecl;
begin
  Result := b2PulleyJointDef_Create;
end;

procedure b2PulleyJointDef_Initialize(_self: Pb2PulleyJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] groundAnchorA: b2Vec2; const [ref] groundAnchorB: b2Vec2; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2PulleyJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PulleyJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] groundAnchorA: b2Vec2; const [ref] groundAnchorB: b2Vec2; const [ref] anchorA: b2Vec2; const [ref] anchorB: b2Vec2; ratio: Single); cdecl;
begin
  b2PulleyJointDef_Initialize(@Self, bodyA, bodyB, groundAnchorA, groundAnchorB, anchorA, anchorB, ratio)
end;


procedure b2PulleyJoint_Destroy(_self: b2PulleyJointHandle); cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PulleyJointWrapper.Destroy; cdecl;
begin
  b2PulleyJoint_Destroy(FHandle);
end;

class operator b2PulleyJointWrapper.Implicit(handle: b2PulleyJointHandle): b2PulleyJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2PulleyJointWrapper.Implicit(wrapper: b2PulleyJointWrapper): b2PulleyJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2PulleyJoint_GetType(_self: b2PulleyJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2PulleyJoint_GetType(FHandle)
end;

function b2PulleyJoint_GetBodyA(_self: b2PulleyJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2PulleyJoint_GetBodyA(FHandle)
end;

function b2PulleyJoint_GetBodyB(_self: b2PulleyJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2PulleyJoint_GetBodyB(FHandle)
end;

function b2PulleyJoint_GetAnchorA(_self: b2PulleyJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2PulleyJoint_GetAnchorA(FHandle)
end;

function b2PulleyJoint_GetAnchorB(_self: b2PulleyJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2PulleyJoint_GetAnchorB(FHandle)
end;

function b2PulleyJoint_GetReactionForce(_self: b2PulleyJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2PulleyJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2PulleyJoint_GetReactionTorque(_self: b2PulleyJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2PulleyJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2PulleyJoint_GetNext(_self: b2PulleyJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2PulleyJoint_GetNext(FHandle)
end;

function b2PulleyJoint_GetUserData(_self: b2PulleyJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2PulleyJoint_GetUserData(FHandle)
end;

procedure b2PulleyJoint_SetUserData(_self: b2PulleyJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PulleyJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2PulleyJoint_SetUserData(FHandle, data)
end;

function b2PulleyJoint_IsActive(_self: b2PulleyJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2PulleyJoint_IsActive(FHandle)
end;

function b2PulleyJoint_GetCollideConnected(_self: b2PulleyJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2PulleyJoint_GetCollideConnected(FHandle)
end;

procedure b2PulleyJoint_Dump(_self: b2PulleyJointHandle); cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PulleyJointWrapper.Dump; cdecl;
begin
  b2PulleyJoint_Dump(FHandle)
end;

procedure b2PulleyJoint_ShiftOrigin(_self: b2PulleyJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PulleyJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2PulleyJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2PulleyJoint_GetGroundAnchorA(_self: b2PulleyJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetGroundAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetGroundAnchorA: b2Vec2; cdecl;
begin
  Result := b2PulleyJoint_GetGroundAnchorA(FHandle)
end;

function b2PulleyJoint_GetGroundAnchorB(_self: b2PulleyJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetGroundAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetGroundAnchorB: b2Vec2; cdecl;
begin
  Result := b2PulleyJoint_GetGroundAnchorB(FHandle)
end;

function b2PulleyJoint_GetLengthA(_self: b2PulleyJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetLengthA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetLengthA: Single; cdecl;
begin
  Result := b2PulleyJoint_GetLengthA(FHandle)
end;

function b2PulleyJoint_GetLengthB(_self: b2PulleyJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetLengthB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetLengthB: Single; cdecl;
begin
  Result := b2PulleyJoint_GetLengthB(FHandle)
end;

function b2PulleyJoint_GetRatio(_self: b2PulleyJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetRatio: Single; cdecl;
begin
  Result := b2PulleyJoint_GetRatio(FHandle)
end;

function b2PulleyJoint_GetCurrentLengthA(_self: b2PulleyJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetCurrentLengthA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetCurrentLengthA: Single; cdecl;
begin
  Result := b2PulleyJoint_GetCurrentLengthA(FHandle)
end;

function b2PulleyJoint_GetCurrentLengthB(_self: b2PulleyJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PulleyJoint_GetCurrentLengthB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PulleyJointWrapper.GetCurrentLengthB: Single; cdecl;
begin
  Result := b2PulleyJoint_GetCurrentLengthB(FHandle)
end;


function b2RevoluteJointDef_Create: b2RevoluteJointDef; cdecl; external LIB_NAME name _PU + 'b2RevoluteJointDef_b2RevoluteJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RevoluteJointDef.Create: b2RevoluteJointDef; cdecl;
begin
  Result := b2RevoluteJointDef_Create;
end;

procedure b2RevoluteJointDef_Initialize(_self: Pb2RevoluteJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2RevoluteJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
begin
  b2RevoluteJointDef_Initialize(@Self, bodyA, bodyB, anchor)
end;


procedure b2RevoluteJoint_Destroy(_self: b2RevoluteJointHandle); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.Destroy; cdecl;
begin
  b2RevoluteJoint_Destroy(FHandle);
end;

class operator b2RevoluteJointWrapper.Implicit(handle: b2RevoluteJointHandle): b2RevoluteJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2RevoluteJointWrapper.Implicit(wrapper: b2RevoluteJointWrapper): b2RevoluteJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2RevoluteJoint_GetType(_self: b2RevoluteJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2RevoluteJoint_GetType(FHandle)
end;

function b2RevoluteJoint_GetBodyA(_self: b2RevoluteJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2RevoluteJoint_GetBodyA(FHandle)
end;

function b2RevoluteJoint_GetBodyB(_self: b2RevoluteJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2RevoluteJoint_GetBodyB(FHandle)
end;

function b2RevoluteJoint_GetAnchorA(_self: b2RevoluteJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2RevoluteJoint_GetAnchorA(FHandle)
end;

function b2RevoluteJoint_GetAnchorB(_self: b2RevoluteJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2RevoluteJoint_GetAnchorB(FHandle)
end;

function b2RevoluteJoint_GetReactionForce(_self: b2RevoluteJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2RevoluteJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2RevoluteJoint_GetReactionTorque(_self: b2RevoluteJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2RevoluteJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2RevoluteJoint_GetNext(_self: b2RevoluteJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2RevoluteJoint_GetNext(FHandle)
end;

function b2RevoluteJoint_GetUserData(_self: b2RevoluteJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2RevoluteJoint_GetUserData(FHandle)
end;

procedure b2RevoluteJoint_SetUserData(_self: b2RevoluteJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2RevoluteJoint_SetUserData(FHandle, data)
end;

function b2RevoluteJoint_IsActive(_self: b2RevoluteJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2RevoluteJoint_IsActive(FHandle)
end;

function b2RevoluteJoint_GetCollideConnected(_self: b2RevoluteJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2RevoluteJoint_GetCollideConnected(FHandle)
end;

procedure b2RevoluteJoint_Dump(_self: b2RevoluteJointHandle); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.Dump; cdecl;
begin
  b2RevoluteJoint_Dump(FHandle)
end;

procedure b2RevoluteJoint_ShiftOrigin(_self: b2RevoluteJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2RevoluteJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2RevoluteJoint_GetLocalAnchorA(_self: b2RevoluteJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2RevoluteJoint_GetLocalAnchorA(FHandle)
end;

function b2RevoluteJoint_GetLocalAnchorB(_self: b2RevoluteJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2RevoluteJoint_GetLocalAnchorB(FHandle)
end;

function b2RevoluteJoint_GetReferenceAngle(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetReferenceAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetReferenceAngle: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetReferenceAngle(FHandle)
end;

function b2RevoluteJoint_GetJointAngle(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetJointAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetJointAngle: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetJointAngle(FHandle)
end;

function b2RevoluteJoint_GetJointSpeed(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetJointSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetJointSpeed: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetJointSpeed(FHandle)
end;

function b2RevoluteJoint_IsLimitEnabled(_self: b2RevoluteJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_IsLimitEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.IsLimitEnabled: Boolean; cdecl;
begin
  Result := b2RevoluteJoint_IsLimitEnabled(FHandle)
end;

procedure b2RevoluteJoint_EnableLimit(_self: b2RevoluteJointHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_EnableLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.EnableLimit(flag: Boolean); cdecl;
begin
  b2RevoluteJoint_EnableLimit(FHandle, flag)
end;

function b2RevoluteJoint_GetLowerLimit(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetLowerLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetLowerLimit: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetLowerLimit(FHandle)
end;

function b2RevoluteJoint_GetUpperLimit(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetUpperLimit'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetUpperLimit: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetUpperLimit(FHandle)
end;

procedure b2RevoluteJoint_SetLimits(_self: b2RevoluteJointHandle; lower: Single; upper: Single); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_SetLimits'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.SetLimits(lower: Single; upper: Single); cdecl;
begin
  b2RevoluteJoint_SetLimits(FHandle, lower, upper)
end;

function b2RevoluteJoint_IsMotorEnabled(_self: b2RevoluteJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_IsMotorEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.IsMotorEnabled: Boolean; cdecl;
begin
  Result := b2RevoluteJoint_IsMotorEnabled(FHandle)
end;

procedure b2RevoluteJoint_EnableMotor(_self: b2RevoluteJointHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_EnableMotor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.EnableMotor(flag: Boolean); cdecl;
begin
  b2RevoluteJoint_EnableMotor(FHandle, flag)
end;

procedure b2RevoluteJoint_SetMotorSpeed(_self: b2RevoluteJointHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_SetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.SetMotorSpeed(speed: Single); cdecl;
begin
  b2RevoluteJoint_SetMotorSpeed(FHandle, speed)
end;

function b2RevoluteJoint_GetMotorSpeed(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetMotorSpeed: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetMotorSpeed(FHandle)
end;

procedure b2RevoluteJoint_SetMaxMotorTorque(_self: b2RevoluteJointHandle; torque: Single); cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_SetMaxMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RevoluteJointWrapper.SetMaxMotorTorque(torque: Single); cdecl;
begin
  b2RevoluteJoint_SetMaxMotorTorque(FHandle, torque)
end;

function b2RevoluteJoint_GetMaxMotorTorque(_self: b2RevoluteJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetMaxMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetMaxMotorTorque: Single; cdecl;
begin
  Result := b2RevoluteJoint_GetMaxMotorTorque(FHandle)
end;

function b2RevoluteJoint_GetMotorTorque(_self: b2RevoluteJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2RevoluteJoint_GetMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RevoluteJointWrapper.GetMotorTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2RevoluteJoint_GetMotorTorque(FHandle, inv_dt)
end;


function b2RopeJointDef_Create: b2RopeJointDef; cdecl; external LIB_NAME name _PU + 'b2RopeJointDef_b2RopeJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RopeJointDef.Create: b2RopeJointDef; cdecl;
begin
  Result := b2RopeJointDef_Create;
end;


procedure b2RopeJoint_Destroy(_self: b2RopeJointHandle); cdecl; external LIB_NAME name _PU + 'b2RopeJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeJointWrapper.Destroy; cdecl;
begin
  b2RopeJoint_Destroy(FHandle);
end;

class operator b2RopeJointWrapper.Implicit(handle: b2RopeJointHandle): b2RopeJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2RopeJointWrapper.Implicit(wrapper: b2RopeJointWrapper): b2RopeJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2RopeJoint_GetType(_self: b2RopeJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2RopeJoint_GetType(FHandle)
end;

function b2RopeJoint_GetBodyA(_self: b2RopeJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2RopeJoint_GetBodyA(FHandle)
end;

function b2RopeJoint_GetBodyB(_self: b2RopeJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2RopeJoint_GetBodyB(FHandle)
end;

function b2RopeJoint_GetAnchorA(_self: b2RopeJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2RopeJoint_GetAnchorA(FHandle)
end;

function b2RopeJoint_GetAnchorB(_self: b2RopeJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2RopeJoint_GetAnchorB(FHandle)
end;

function b2RopeJoint_GetReactionForce(_self: b2RopeJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2RopeJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2RopeJoint_GetReactionTorque(_self: b2RopeJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2RopeJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2RopeJoint_GetNext(_self: b2RopeJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2RopeJoint_GetNext(FHandle)
end;

function b2RopeJoint_GetUserData(_self: b2RopeJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2RopeJoint_GetUserData(FHandle)
end;

procedure b2RopeJoint_SetUserData(_self: b2RopeJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2RopeJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2RopeJoint_SetUserData(FHandle, data)
end;

function b2RopeJoint_IsActive(_self: b2RopeJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2RopeJoint_IsActive(FHandle)
end;

function b2RopeJoint_GetCollideConnected(_self: b2RopeJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2RopeJoint_GetCollideConnected(FHandle)
end;

procedure b2RopeJoint_Dump(_self: b2RopeJointHandle); cdecl; external LIB_NAME name _PU + 'b2RopeJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeJointWrapper.Dump; cdecl;
begin
  b2RopeJoint_Dump(FHandle)
end;

procedure b2RopeJoint_ShiftOrigin(_self: b2RopeJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2RopeJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2RopeJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2RopeJoint_GetLocalAnchorA(_self: b2RopeJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2RopeJoint_GetLocalAnchorA(FHandle)
end;

function b2RopeJoint_GetLocalAnchorB(_self: b2RopeJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2RopeJoint_GetLocalAnchorB(FHandle)
end;

procedure b2RopeJoint_SetMaxLength(_self: b2RopeJointHandle; length: Single); cdecl; external LIB_NAME name _PU + 'b2RopeJoint_SetMaxLength'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeJointWrapper.SetMaxLength(length: Single); cdecl;
begin
  b2RopeJoint_SetMaxLength(FHandle, length)
end;

function b2RopeJoint_GetMaxLength(_self: b2RopeJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetMaxLength'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetMaxLength: Single; cdecl;
begin
  Result := b2RopeJoint_GetMaxLength(FHandle)
end;

function b2RopeJoint_GetLimitState(_self: b2RopeJointHandle): b2LimitState; cdecl; external LIB_NAME name _PU + 'b2RopeJoint_GetLimitState'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeJointWrapper.GetLimitState: b2LimitState; cdecl;
begin
  Result := b2RopeJoint_GetLimitState(FHandle)
end;


function b2WeldJointDef_Create: b2WeldJointDef; cdecl; external LIB_NAME name _PU + 'b2WeldJointDef_b2WeldJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2WeldJointDef.Create: b2WeldJointDef; cdecl;
begin
  Result := b2WeldJointDef_Create;
end;

procedure b2WeldJointDef_Initialize(_self: Pb2WeldJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2WeldJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2); cdecl;
begin
  b2WeldJointDef_Initialize(@Self, bodyA, bodyB, anchor)
end;


procedure b2WeldJoint_Destroy(_self: b2WeldJointHandle); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.Destroy; cdecl;
begin
  b2WeldJoint_Destroy(FHandle);
end;

class operator b2WeldJointWrapper.Implicit(handle: b2WeldJointHandle): b2WeldJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2WeldJointWrapper.Implicit(wrapper: b2WeldJointWrapper): b2WeldJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2WeldJoint_GetType(_self: b2WeldJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2WeldJoint_GetType(FHandle)
end;

function b2WeldJoint_GetBodyA(_self: b2WeldJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2WeldJoint_GetBodyA(FHandle)
end;

function b2WeldJoint_GetBodyB(_self: b2WeldJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2WeldJoint_GetBodyB(FHandle)
end;

function b2WeldJoint_GetAnchorA(_self: b2WeldJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2WeldJoint_GetAnchorA(FHandle)
end;

function b2WeldJoint_GetAnchorB(_self: b2WeldJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2WeldJoint_GetAnchorB(FHandle)
end;

function b2WeldJoint_GetReactionForce(_self: b2WeldJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2WeldJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2WeldJoint_GetReactionTorque(_self: b2WeldJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2WeldJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2WeldJoint_GetNext(_self: b2WeldJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2WeldJoint_GetNext(FHandle)
end;

function b2WeldJoint_GetUserData(_self: b2WeldJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2WeldJoint_GetUserData(FHandle)
end;

procedure b2WeldJoint_SetUserData(_self: b2WeldJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2WeldJoint_SetUserData(FHandle, data)
end;

function b2WeldJoint_IsActive(_self: b2WeldJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2WeldJoint_IsActive(FHandle)
end;

function b2WeldJoint_GetCollideConnected(_self: b2WeldJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2WeldJoint_GetCollideConnected(FHandle)
end;

procedure b2WeldJoint_Dump(_self: b2WeldJointHandle); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.Dump; cdecl;
begin
  b2WeldJoint_Dump(FHandle)
end;

procedure b2WeldJoint_ShiftOrigin(_self: b2WeldJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2WeldJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2WeldJoint_GetLocalAnchorA(_self: b2WeldJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2WeldJoint_GetLocalAnchorA(FHandle)
end;

function b2WeldJoint_GetLocalAnchorB(_self: b2WeldJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2WeldJoint_GetLocalAnchorB(FHandle)
end;

function b2WeldJoint_GetReferenceAngle(_self: b2WeldJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetReferenceAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetReferenceAngle: Single; cdecl;
begin
  Result := b2WeldJoint_GetReferenceAngle(FHandle)
end;

procedure b2WeldJoint_SetFrequency(_self: b2WeldJointHandle; hz: Single); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_SetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.SetFrequency(hz: Single); cdecl;
begin
  b2WeldJoint_SetFrequency(FHandle, hz)
end;

function b2WeldJoint_GetFrequency(_self: b2WeldJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetFrequency'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetFrequency: Single; cdecl;
begin
  Result := b2WeldJoint_GetFrequency(FHandle)
end;

procedure b2WeldJoint_SetDampingRatio(_self: b2WeldJointHandle; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2WeldJoint_SetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WeldJointWrapper.SetDampingRatio(ratio: Single); cdecl;
begin
  b2WeldJoint_SetDampingRatio(FHandle, ratio)
end;

function b2WeldJoint_GetDampingRatio(_self: b2WeldJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WeldJoint_GetDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WeldJointWrapper.GetDampingRatio: Single; cdecl;
begin
  Result := b2WeldJoint_GetDampingRatio(FHandle)
end;


function b2WheelJointDef_Create: b2WheelJointDef; cdecl; external LIB_NAME name _PU + 'b2WheelJointDef_b2WheelJointDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2WheelJointDef.Create: b2WheelJointDef; cdecl;
begin
  Result := b2WheelJointDef_Create;
end;

procedure b2WheelJointDef_Initialize(_self: Pb2WheelJointDef; bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2WheelJointDef_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointDef.Initialize(bodyA: b2BodyHandle; bodyB: b2BodyHandle; const [ref] anchor: b2Vec2; const [ref] axis: b2Vec2); cdecl;
begin
  b2WheelJointDef_Initialize(@Self, bodyA, bodyB, anchor, axis)
end;


procedure b2WheelJoint_Destroy(_self: b2WheelJointHandle); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.Destroy; cdecl;
begin
  b2WheelJoint_Destroy(FHandle);
end;

class operator b2WheelJointWrapper.Implicit(handle: b2WheelJointHandle): b2WheelJointWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2WheelJointWrapper.Implicit(wrapper: b2WheelJointWrapper): b2WheelJointHandle;
begin
  Result := wrapper.FHandle;
end;

function b2WheelJoint_GetType(_self: b2WheelJointHandle): b2JointType; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetType: b2JointType; cdecl;
begin
  Result := b2WheelJoint_GetType(FHandle)
end;

function b2WheelJoint_GetBodyA(_self: b2WheelJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetBodyA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetBodyA: b2BodyHandle; cdecl;
begin
  Result := b2WheelJoint_GetBodyA(FHandle)
end;

function b2WheelJoint_GetBodyB(_self: b2WheelJointHandle): b2BodyHandle; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetBodyB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetBodyB: b2BodyHandle; cdecl;
begin
  Result := b2WheelJoint_GetBodyB(FHandle)
end;

function b2WheelJoint_GetAnchorA(_self: b2WheelJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetAnchorA: b2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetAnchorA(FHandle)
end;

function b2WheelJoint_GetAnchorB(_self: b2WheelJointHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetAnchorB: b2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetAnchorB(FHandle)
end;

function b2WheelJoint_GetReactionForce(_self: b2WheelJointHandle; inv_dt: Single): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetReactionForce'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetReactionForce(inv_dt: Single): b2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetReactionForce(FHandle, inv_dt)
end;

function b2WheelJoint_GetReactionTorque(_self: b2WheelJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetReactionTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetReactionTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2WheelJoint_GetReactionTorque(FHandle, inv_dt)
end;

function b2WheelJoint_GetNext(_self: b2WheelJointHandle): b2JointHandle; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetNext'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetNext: b2JointHandle; cdecl;
begin
  Result := b2WheelJoint_GetNext(FHandle)
end;

function b2WheelJoint_GetUserData(_self: b2WheelJointHandle): Pointer; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetUserData: Pointer; cdecl;
begin
  Result := b2WheelJoint_GetUserData(FHandle)
end;

procedure b2WheelJoint_SetUserData(_self: b2WheelJointHandle; data: Pointer); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_SetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.SetUserData(data: Pointer); cdecl;
begin
  b2WheelJoint_SetUserData(FHandle, data)
end;

function b2WheelJoint_IsActive(_self: b2WheelJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_IsActive'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.IsActive: Boolean; cdecl;
begin
  Result := b2WheelJoint_IsActive(FHandle)
end;

function b2WheelJoint_GetCollideConnected(_self: b2WheelJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetCollideConnected'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetCollideConnected: Boolean; cdecl;
begin
  Result := b2WheelJoint_GetCollideConnected(FHandle)
end;

procedure b2WheelJoint_Dump(_self: b2WheelJointHandle); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_Dump'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.Dump; cdecl;
begin
  b2WheelJoint_Dump(FHandle)
end;

procedure b2WheelJoint_ShiftOrigin(_self: b2WheelJointHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2WheelJoint_ShiftOrigin(FHandle, newOrigin)
end;

function b2WheelJoint_GetLocalAnchorA(_self: b2WheelJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetLocalAnchorA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetLocalAnchorA: Pb2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetLocalAnchorA(FHandle)
end;

function b2WheelJoint_GetLocalAnchorB(_self: b2WheelJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetLocalAnchorB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetLocalAnchorB: Pb2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetLocalAnchorB(FHandle)
end;

function b2WheelJoint_GetLocalAxisA(_self: b2WheelJointHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetLocalAxisA'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetLocalAxisA: Pb2Vec2; cdecl;
begin
  Result := b2WheelJoint_GetLocalAxisA(FHandle)
end;

function b2WheelJoint_GetJointTranslation(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetJointTranslation'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetJointTranslation: Single; cdecl;
begin
  Result := b2WheelJoint_GetJointTranslation(FHandle)
end;

function b2WheelJoint_GetJointSpeed(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetJointSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetJointSpeed: Single; cdecl;
begin
  Result := b2WheelJoint_GetJointSpeed(FHandle)
end;

function b2WheelJoint_IsMotorEnabled(_self: b2WheelJointHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_IsMotorEnabled'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.IsMotorEnabled: Boolean; cdecl;
begin
  Result := b2WheelJoint_IsMotorEnabled(FHandle)
end;

procedure b2WheelJoint_EnableMotor(_self: b2WheelJointHandle; flag: Boolean); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_EnableMotor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.EnableMotor(flag: Boolean); cdecl;
begin
  b2WheelJoint_EnableMotor(FHandle, flag)
end;

procedure b2WheelJoint_SetMotorSpeed(_self: b2WheelJointHandle; speed: Single); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_SetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.SetMotorSpeed(speed: Single); cdecl;
begin
  b2WheelJoint_SetMotorSpeed(FHandle, speed)
end;

function b2WheelJoint_GetMotorSpeed(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetMotorSpeed'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetMotorSpeed: Single; cdecl;
begin
  Result := b2WheelJoint_GetMotorSpeed(FHandle)
end;

procedure b2WheelJoint_SetMaxMotorTorque(_self: b2WheelJointHandle; torque: Single); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_SetMaxMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.SetMaxMotorTorque(torque: Single); cdecl;
begin
  b2WheelJoint_SetMaxMotorTorque(FHandle, torque)
end;

function b2WheelJoint_GetMaxMotorTorque(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetMaxMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetMaxMotorTorque: Single; cdecl;
begin
  Result := b2WheelJoint_GetMaxMotorTorque(FHandle)
end;

function b2WheelJoint_GetMotorTorque(_self: b2WheelJointHandle; inv_dt: Single): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetMotorTorque'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetMotorTorque(inv_dt: Single): Single; cdecl;
begin
  Result := b2WheelJoint_GetMotorTorque(FHandle, inv_dt)
end;

procedure b2WheelJoint_SetSpringFrequencyHz(_self: b2WheelJointHandle; hz: Single); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_SetSpringFrequencyHz'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.SetSpringFrequencyHz(hz: Single); cdecl;
begin
  b2WheelJoint_SetSpringFrequencyHz(FHandle, hz)
end;

function b2WheelJoint_GetSpringFrequencyHz(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetSpringFrequencyHz'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetSpringFrequencyHz: Single; cdecl;
begin
  Result := b2WheelJoint_GetSpringFrequencyHz(FHandle)
end;

procedure b2WheelJoint_SetSpringDampingRatio(_self: b2WheelJointHandle; ratio: Single); cdecl; external LIB_NAME name _PU + 'b2WheelJoint_SetSpringDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WheelJointWrapper.SetSpringDampingRatio(ratio: Single); cdecl;
begin
  b2WheelJoint_SetSpringDampingRatio(FHandle, ratio)
end;

function b2WheelJoint_GetSpringDampingRatio(_self: b2WheelJointHandle): Single; cdecl; external LIB_NAME name _PU + 'b2WheelJoint_GetSpringDampingRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2WheelJointWrapper.GetSpringDampingRatio: Single; cdecl;
begin
  Result := b2WheelJoint_GetSpringDampingRatio(FHandle)
end;


function b2MixFriction(friction1: Single; friction2: Single): Single; cdecl; external LIB_NAME name _PU + 'Dynamics_b2MixFriction'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MixRestitution(restitution1: Single; restitution2: Single): Single; cdecl; external LIB_NAME name _PU + 'Dynamics_b2MixRestitution'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


end.
