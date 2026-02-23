{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleScene, CastleSceneCore, CastleTransform, CastleCameras,
  CastleLivingBehaviors, CastleFlashEffect, CastleBehaviors,
  CastleColors, X3DNodes;

type
  TSnowFlake = record
    Transform: TCastleTransformReference;
    Speed: Single;
    DriftPhase: Single;
    DriftSpeed: Single;
  end;

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    PlayerLiving: TCastleLiving;
    PlayerHurtFlash: TCastleFlashEffect;
    LabelPlayerLife: TCastleLabel;
    LabelQuest: TCastleLabel;
    LabelTalkHint: TCastleLabel;
    LabelNotification: TCastleLabel;
    LabelMouseLook: TCastleLabel;
    LabelQuestCompleted: TCastleLabel;
    SceneNpc1: TCastleScene;
    SceneNpc2: TCastleScene;
  private
    SnowTemplate: TCastleTransform;
    SnowFlakes: array [0..399] of TSnowFlake;
    SnowInitialized: Boolean;
    GoblinKillCount: Integer;
    NotificationTimer: Single;
    MapViewport: TCastleViewport;
    MapCamera: TCastleCamera;
    MapBorder: TCastleRectangleControl;
    SwordTransform: TCastleTransform;
    SwordSwinging: Boolean;
    SwordSwingTimer: Single;
    KillQuestCompleted: Boolean;
    QuestCompletedTimer: Single;
    TrophyTransform: TCastleTransform;
    procedure PlayerHurt(Sender: TObject);
    procedure InitSnow;
    procedure UpdateSnow(const SecondsPassed: Single);
    procedure ShowNotification(const Msg: String);
    function GetNpcDialogId(const NpcName: String): Integer;
    procedure CreateMinimap;
    procedure CreateBeacons;
    procedure CreateSword;
    procedure SpawnTrophy;
    procedure UpdateMinimap;
  public
    MouseLookActive: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleLog, CastleSoundEngine, CastleRenderOptions, CastleProjection,
  GameNpcs, GameQuests, GameDialog, GameViewDialog;

const
  SwordSwingDuration = 0.3;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  Npc: TNpcBehavior;
begin
  inherited;
  PlayerLiving.OnHurt := {$ifdef FPC}@{$endif} PlayerHurt;
  GoblinKillCount := 0;
  NotificationTimer := 0;
  SnowInitialized := false;
  LabelTalkHint.Exists := false;
  LabelNotification.Exists := false;
  LabelQuestCompleted.Exists := false;

  { Mouse look }
  MouseLookActive := true;
  WalkNavigation.MouseLook := true;

  { Quest completion state }
  KillQuestCompleted := false;
  QuestCompletedTimer := 0;

  { Sword state }
  SwordSwinging := false;

  { Attach NPC behaviors }
  Npc := TNpcBehavior.Create(FreeAtStop);
  Npc.NpcName := 'Captain Aldric';
  Npc.DialogId := 0;
  SceneNpc1.AddBehavior(Npc);

  Npc := TNpcBehavior.Create(FreeAtStop);
  Npc.NpcName := 'Scout Mira';
  Npc.DialogId := 4;
  SceneNpc2.AddBehavior(Npc);

  CreateBeacons;
  CreateMinimap;
  CreateSword;
end;

procedure TViewMain.CreateSword;
var
  Blade: TCastleCone;
  Handle: TCastleCylinder;
  Guard: TCastleBox;
  BladeT, HandleT, GuardT: TCastleTransform;
begin
  SwordTransform := TCastleTransform.Create(FreeAtStop);
  SwordTransform.Exists := false;

  { Blade }
  Blade := TCastleCone.Create(FreeAtStop);
  Blade.BottomRadius := 0.025;
  Blade.Height := 0.7;
  Blade.Slices := 6;
  Blade.Color := Vector4(0.75, 0.75, 0.8, 1.0);
  Blade.Material := pmPhysical;

  { Handle }
  Handle := TCastleCylinder.Create(FreeAtStop);
  Handle.Radius := 0.02;
  Handle.Height := 0.15;
  Handle.Color := Vector4(0.4, 0.25, 0.1, 1.0);
  Handle.Material := pmPhysical;

  { Guard (cross-piece) }
  Guard := TCastleBox.Create(FreeAtStop);
  Guard.Size := Vector3(0.12, 0.02, 0.02);
  Guard.Color := Vector4(0.6, 0.5, 0.2, 1.0);
  Guard.Material := pmPhysical;

  { Assemble: blade on top, handle below, guard at junction }
  BladeT := TCastleTransform.Create(FreeAtStop);
  BladeT.Translation := Vector3(0, 0.15, 0);
  BladeT.Add(Blade);

  HandleT := TCastleTransform.Create(FreeAtStop);
  HandleT.Translation := Vector3(0, 0, 0);
  HandleT.Add(Handle);

  GuardT := TCastleTransform.Create(FreeAtStop);
  GuardT.Translation := Vector3(0, 0.14, 0);
  GuardT.Add(Guard);

  SwordTransform.Add(BladeT);
  SwordTransform.Add(HandleT);
  SwordTransform.Add(GuardT);

  { Position sword in lower-right like a held weapon }
  SwordTransform.Translation := Vector3(0.25, -0.45, -0.4);
  SwordTransform.Rotation := Vector4(1, 0, 0.3, -0.3);

  SwordTransform.Collides := false;
  SwordTransform.Pickable := false;

  { Parent to camera so it moves with player view }
  MainViewport.Camera.Add(SwordTransform);
end;

procedure TViewMain.SpawnTrophy;
var
  GrailScene: TCastleScene;
  CamPos, CamDir: TVector3;
begin
  GrailScene := TCastleScene.Create(FreeAtStop);
  GrailScene.Url := 'castle-data:/grail.glb';

  TrophyTransform := TCastleTransform.Create(FreeAtStop);
  TrophyTransform.Add(GrailScene);
  TrophyTransform.Scale := Vector3(3, 3, 3);

  { Place 4 meters in front of the player }
  CamPos := MainViewport.Camera.WorldTranslation;
  CamDir := MainViewport.Camera.Direction;
  CamDir.Y := 0;
  if not CamDir.IsZero then
    CamDir := CamDir.Normalize;
  TrophyTransform.Translation := Vector3(
    CamPos.X + CamDir.X * 4,
    CamPos.Y - 0.5,
    CamPos.Z + CamDir.Z * 4
  );

  TrophyTransform.Collides := false;
  TrophyTransform.Pickable := false;

  MainViewport.Items.Add(TrophyTransform);
end;

procedure TViewMain.CreateBeacons;

  procedure AddBeacon(const Pos: TVector3; const Col: TCastleColor;
    const AHeight: Single; const ARadius: Single);
  var
    Cyl: TCastleCylinder;
    T: TCastleTransform;
  begin
    Cyl := TCastleCylinder.Create(FreeAtStop);
    Cyl.Radius := ARadius;
    Cyl.Height := AHeight;
    Cyl.Color := Col;
    Cyl.Material := pmUnlit;

    T := TCastleTransform.Create(FreeAtStop);
    T.Translation := Pos;
    T.Add(Cyl);
    T.Collides := false;
    T.Pickable := false;
    MainViewport.Items.Add(T);
  end;

var
  YellowBeacon, RedBeacon, BlueBeacon: TCastleColor;
begin
  YellowBeacon := Vector4(1.0, 0.85, 0.2, 1.0);
  RedBeacon := Vector4(1.0, 0.2, 0.15, 1.0);
  BlueBeacon := Vector4(0.2, 0.5, 1.0, 1.0);

  { Campfire beacon - tall yellow pillar }
  AddBeacon(Vector3(15, 0, 35), YellowBeacon, 15, 0.3);

  { Goblin area beacons - smaller red pillars }
  AddBeacon(Vector3(40, 0, 10), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(-35, 0, 20), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(50, 0, -30), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(-45, 0, -25), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(30, 0, 50), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(-20, 0, -50), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(60, 0, 30), RedBeacon, 10, 0.2);
  AddBeacon(Vector3(-55, 0, 40), RedBeacon, 10, 0.2);

  { River beacon - blue pillar }
  AddBeacon(Vector3(80, 0, 0), BlueBeacon, 20, 0.4);
end;

procedure TViewMain.CreateMinimap;
var
  MapLabel: TCastleLabel;
begin
  { Background border }
  MapBorder := TCastleRectangleControl.Create(FreeAtStop);
  MapBorder.Width := 210;
  MapBorder.Height := 230;
  MapBorder.Color := Vector4(0, 0, 0, 0.6);
  MapBorder.Anchor(hpRight, -5);
  MapBorder.Anchor(vpBottom, 5);
  InsertFront(MapBorder);

  { "Map" label }
  MapLabel := TCastleLabel.Create(FreeAtStop);
  MapLabel.Caption := 'MAP';
  MapLabel.Color := White;
  MapLabel.FontSize := 14;
  MapLabel.Anchor(hpMiddle);
  MapLabel.Anchor(vpTop, -3);
  MapBorder.InsertFront(MapLabel);

  { Camera for top-down view }
  MapCamera := TCastleCamera.Create(FreeAtStop);
  MapCamera.ProjectionType := ptOrthographic;
  MapCamera.Orthographic.Width := 200;
  MapCamera.Orthographic.Origin := Vector2(0.5, 0.5);
  MainViewport.Items.Add(MapCamera);

  { Minimap viewport }
  MapViewport := TCastleViewport.Create(FreeAtStop);
  MapViewport.Width := 200;
  MapViewport.Height := 200;
  MapViewport.Anchor(hpRight, -10);
  MapViewport.Anchor(vpBottom, 10);
  MapViewport.Items := MainViewport.Items;
  MapViewport.Camera := MapCamera;
  MapViewport.Transparent := true;
  InsertFront(MapViewport);
end;

procedure TViewMain.UpdateMinimap;
var
  CamPos, DirHoriz: TVector3;
begin
  if MapCamera = nil then Exit;

  CamPos := MainViewport.Camera.WorldTranslation;

  { Get horizontal direction for map orientation }
  DirHoriz := MainViewport.Camera.Direction;
  DirHoriz.Y := 0;
  if DirHoriz.IsZero then
    DirHoriz := Vector3(0, 0, -1)
  else
    DirHoriz := DirHoriz.Normalize;

  MapCamera.SetView(
    Vector3(CamPos.X, 120, CamPos.Z),
    Vector3(0, -1, 0),
    DirHoriz
  );
end;

procedure TViewMain.InitSnow;
var
  I: Integer;
  Sphere: TCastleSphere;
  CamPos: TVector3;
begin
  CamPos := MainViewport.Camera.WorldTranslation;

  { Create a single shared snowflake sphere }
  SnowTemplate := TCastleTransform.Create(FreeAtStop);
  Sphere := TCastleSphere.Create(FreeAtStop);
  Sphere.Radius := 0.05;
  Sphere.Slices := 4;
  Sphere.Stacks := 2;
  Sphere.Color := Vector4(0.95, 0.95, 1.0, 0.85);
  Sphere.Material := pmUnlit;
  SnowTemplate.Add(Sphere);
  SnowTemplate.Collides := false;
  SnowTemplate.Pickable := false;
  MainViewport.Items.Add(SnowTemplate);

  for I := 0 to High(SnowFlakes) do
  begin
    SnowFlakes[I].Transform := TCastleTransformReference.Create(FreeAtStop);
    SnowFlakes[I].Transform.Reference := SnowTemplate;
    SnowFlakes[I].Transform.Scale := Vector3(0.8 + Random * 0.6, 0.8 + Random * 0.6, 0.8 + Random * 0.6);
    SnowFlakes[I].Transform.Translation := Vector3(
      CamPos.X + Random * 60 - 30,
      CamPos.Y + Random * 25,
      CamPos.Z + Random * 60 - 30
    );
    SnowFlakes[I].Transform.Collides := false;
    SnowFlakes[I].Transform.Pickable := false;
    SnowFlakes[I].Speed := 1.5 + Random * 2.0;
    SnowFlakes[I].DriftPhase := Random * 2 * Pi;
    SnowFlakes[I].DriftSpeed := 0.5 + Random * 1.0;

    MainViewport.Items.Add(SnowFlakes[I].Transform);
  end;
  SnowInitialized := true;
end;

procedure TViewMain.UpdateSnow(const SecondsPassed: Single);
var
  I: Integer;
  Pos, CamPos: TVector3;
  T: Single;
begin
  CamPos := MainViewport.Camera.WorldTranslation;
  for I := 0 to High(SnowFlakes) do
  begin
    Pos := SnowFlakes[I].Transform.Translation;

    { Fall down }
    Pos.Y := Pos.Y - SnowFlakes[I].Speed * SecondsPassed;

    { Gentle drift }
    T := SnowFlakes[I].DriftPhase + SnowFlakes[I].DriftSpeed * SecondsPassed;
    SnowFlakes[I].DriftPhase := T;
    Pos.X := Pos.X + Sin(T) * 0.3 * SecondsPassed;
    Pos.Z := Pos.Z + Cos(T * 0.7) * 0.2 * SecondsPassed;

    { Recycle if too low or too far from camera }
    if (Pos.Y < CamPos.Y - 5) or
       (Abs(Pos.X - CamPos.X) > 35) or
       (Abs(Pos.Z - CamPos.Z) > 35) then
    begin
      Pos.X := CamPos.X + Random * 60 - 30;
      Pos.Y := CamPos.Y + 15 + Random * 15;
      Pos.Z := CamPos.Z + Random * 60 - 30;
      SnowFlakes[I].Speed := 1.5 + Random * 2.0;
    end;

    SnowFlakes[I].Transform.Translation := Pos;
  end;
end;

procedure TViewMain.ShowNotification(const Msg: String);
begin
  LabelNotification.Caption := Msg;
  LabelNotification.Exists := true;
  NotificationTimer := 3.0;
end;

function TViewMain.GetNpcDialogId(const NpcName: String): Integer;
var
  Q: TQuest;
begin
  if NpcName = 'Captain Aldric' then
  begin
    Q := QuestManager.GetQuest(0);
    case Q.State of
      qsPending: Result := 0;
      qsActive: Result := 2;
      qsCompleted: Result := 3;
    end;
  end
  else if NpcName = 'Scout Mira' then
  begin
    Q := QuestManager.GetQuest(1);
    case Q.State of
      qsPending: Result := 4;
      qsActive: Result := 6;
      qsCompleted: Result := 7;
    end;
  end
  else
    Result := -1;
end;

procedure TViewMain.PlayerHurt(Sender: TObject);
begin
  PlayerHurtFlash.Flash(Red, true);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  HitTransform: TCastleTransform;
  QuestCompletedId: Integer;
  SwingT: Single;
begin
  inherited;
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Initialize snow on first frame }
  if not SnowInitialized then
    InitSnow;

  { Update snow particles }
  UpdateSnow(SecondsPassed);

  { Update minimap }
  UpdateMinimap;

  { Update health display }
  LabelPlayerLife.Caption := 'Life: ' + IntToStr(Round(PlayerLiving.Life));

  { Update quest display }
  LabelQuest.Caption := QuestManager.GetActiveQuestText;

  { Update mouse look label }
  if MouseLookActive then
    LabelMouseLook.Caption := 'Mouse Look ON (Esc or Right-Click to release)'
  else
    LabelMouseLook.Caption := 'Mouse Look OFF (Right-Click to enable)';

  { Animate sword swing }
  if SwordSwinging then
  begin
    SwordSwingTimer := SwordSwingTimer + SecondsPassed;
    if SwordSwingTimer < SwordSwingDuration then
    begin
      { Swing from right to left across screen }
      SwingT := SwordSwingTimer / SwordSwingDuration;
      SwordTransform.Translation := Vector3(
        0.25 - SwingT * 0.3,
        -0.45 + Sin(SwingT * Pi) * 0.15,
        -0.4
      );
      SwordTransform.Rotation := Vector4(0, 0, 1, -0.5 + SwingT * 1.2);
    end
    else
    begin
      SwordSwinging := false;
      SwordTransform.Exists := false;
    end;
  end;

  { Check quest completion notifications }
  QuestCompletedId := QuestManager.LastCompletedQuest;
  if QuestCompletedId >= 0 then
  begin
    ShowNotification('Quest completed: ' + QuestManager.GetQuest(QuestCompletedId).Name + '!');
    QuestManager.ClearLastCompleted;

    { Special handling for kill quest }
    if (QuestCompletedId = 0) and (not KillQuestCompleted) then
    begin
      KillQuestCompleted := true;
      LabelQuestCompleted.Exists := true;
      QuestCompletedTimer := 5.0;
      SpawnTrophy;
    end;
  end;

  { Check scout location }
  QuestManager.CheckScoutLocation(MainViewport.Camera.WorldTranslation);

  { Notification timer }
  if NotificationTimer > 0 then
  begin
    NotificationTimer := NotificationTimer - SecondsPassed;
    if NotificationTimer <= 0 then
      LabelNotification.Exists := false;
  end;

  { Quest completed label timer }
  if QuestCompletedTimer > 0 then
  begin
    QuestCompletedTimer := QuestCompletedTimer - SecondsPassed;
    if QuestCompletedTimer <= 0 then
      LabelQuestCompleted.Exists := false;
  end;

  { Check if looking at NPC (for talk hint) }
  HitTransform := MainViewport.TransformHit(
    Vector2(MainViewport.EffectiveWidth / 2, MainViewport.EffectiveHeight / 2), false);
  if (HitTransform <> nil) and
     (HitTransform.FindBehavior(TNpcBehavior) <> nil) then
    LabelTalkHint.Exists := true
  else
    LabelTalkHint.Exists := false;

  { Check if player is dead }
  if PlayerLiving.Dead then
  begin
    PlayerLiving.Life := PlayerLiving.MaxLife;
    ShowNotification('You have been revived!');
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  HitTransform: TCastleTransform;
  HitLiving: TCastleLiving;
  HitScene: TCastleScene;
  NpcBehavior: TNpcBehavior;
begin
  Result := inherited;
  if Result then Exit;

  { Escape: toggle mouse look off }
  if Event.IsKey(keyEscape) then
  begin
    if MouseLookActive then
    begin
      MouseLookActive := false;
      WalkNavigation.MouseLook := false;
      Exit(true);
    end;
  end;

  { Right-click: toggle mouse look }
  if Event.IsMouseButton(buttonRight) then
  begin
    MouseLookActive := not MouseLookActive;
    WalkNavigation.MouseLook := MouseLookActive;
    Exit(true);
  end;

  { Left-click: attack }
  if Event.IsMouseButton(buttonLeft) then
  begin
    { Start sword swing visual }
    if not SwordSwinging then
    begin
      SwordSwinging := true;
      SwordSwingTimer := 0;
      SwordTransform.Exists := true;
      SwordTransform.Translation := Vector3(0.25, -0.45, -0.4);
      SwordTransform.Rotation := Vector4(0, 0, 1, -0.5);
    end;

    { Raycast for hit detection }
    HitTransform := MainViewport.TransformHit(
      Vector2(MainViewport.EffectiveWidth / 2, MainViewport.EffectiveHeight / 2), false);
    if (HitTransform <> nil) and
       (HitTransform.FindBehavior(TCastleLiving) <> nil) then
    begin
      HitLiving := HitTransform.FindBehavior(TCastleLiving) as TCastleLiving;
      if HitLiving <> PlayerLiving then
      begin
        HitLiving.Hurt(25 + Random(15), MainViewport.Camera.WorldDirection, 0, PlayerLiving);
        if HitLiving.Dead then
        begin
          if HitTransform is TCastleScene then
          begin
            HitScene := HitTransform as TCastleScene;
            HitScene.Pickable := false;
            HitScene.Collides := false;
            { Play death animation }
            HitScene.PlayAnimation('die', false);
          end;
          Inc(GoblinKillCount);
          QuestManager.AddKill;
          ShowNotification('Goblin slain! (' + IntToStr(GoblinKillCount) + ' total)');
        end
        else
        begin
          { Play hurt animation }
          if HitTransform is TCastleScene then
            (HitTransform as TCastleScene).PlayAnimation('hurt', false);
        end;
      end;
    end;
    Exit(true);
  end;

  { E key: interact with NPC }
  if Event.IsKey(keyE) then
  begin
    HitTransform := MainViewport.TransformHit(
      Vector2(MainViewport.EffectiveWidth / 2, MainViewport.EffectiveHeight / 2), false);
    if (HitTransform <> nil) then
    begin
      NpcBehavior := HitTransform.FindBehavior(TNpcBehavior) as TNpcBehavior;
      if NpcBehavior <> nil then
      begin
        ViewDialog.CurrentDialogId := GetNpcDialogId(NpcBehavior.NpcName);
        Container.PushView(ViewDialog);
        Exit(true);
      end;
    end;
  end;
end;

end.
