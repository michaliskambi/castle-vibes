# Castle Game Engine - "Castle Vibes: The Winter Siege"

## Project Overview

A first-person medieval action game built with Castle Game Engine. The player arrives at a castle outpost under goblin siege. Features combat with sword animation, animated goblin enemies, NPC dialog, quests, and a trophy reward system.

## Project Structure

- `CastleEngineManifest.xml` - Project manifest
- `code/gameinitialize.pas` - Application initialization, creates views, initializes game systems
- `code/gameviewtitle.pas` - Title screen "Castle Vibes" with start button
- `code/gameviewmain.pas` - Main view: combat, snow particles, NPC interaction, quest HUD, minimap, sword, trophy
- `code/gameviewdialog.pas` - Dialog overlay view for NPC conversations
- `code/gamedialog.pas` - Dialog tree data (8 dialog entries for 2 NPCs)
- `code/gamenpcs.pas` - TNpcBehavior class for talkable NPCs
- `code/gamequests.pas` - TQuestManager with 2 quests (kill goblins, scout river)
- `data/gameviewmain.castle-user-interface` - JSON design file (scene, UI, behaviors)
- `data/CastleSettings.xml` - Container settings
- `generate_castle_glb.py` - Python script generating all 3D models (with glTF animation support)
- `data/castle.glb` - Castle + terrain + trees (~5.8MB)
- `data/goblin.glb` - Animated goblin with idle/move/die/hurt animations
- `data/npc.glb` - Simple NPC humanoid model
- `data/campfire.glb` - Campfire with stone ring and flame
- `data/grail.glb` - Golden grail trophy (quest reward)

## Building and Running

```bash
castle-engine compile    # Build
castle-engine run        # Compile + run
castle-engine clean      # Clean build artifacts
python3 generate_castle_glb.py  # Regenerate all 3D models
```

## Controls

- **WASD** - Move
- **Mouse** - Look around (when mouse look is ON)
- **Left Click** - Attack with sword (hit goblin in crosshair to damage it)
- **Right Click** - Toggle mouse look on/off
- **Escape** - Turn mouse look off / close dialog
- **E** - Talk to NPC (when "Press E to talk" hint appears)

## Game Features

### Title Screen
- "Castle Vibes" title in yellow, "The Winter Siege" subtitle
- "Start Game" button transitions to main gameplay

### Environment
- Wintery cloudy sky (TCastleBackground with steel blue/gray gradient)
- Distance fog (TCastleFog, linear, 180m range)
- 400 snow particles that fall and drift around the player
- Heightmap terrain (500x500m, varied hills/valleys, grass texture)
- ~300 trees (oak, pine, birch types with multi-layered canopies)
- River valley to the east (translucent blue TCastlePlane at X=80)
- Minimap (top-down orthographic viewport, bottom-right corner)
- Colored beacon pillars: yellow (campfire), red (goblins), blue (river)

### Castle
- Multi-layered medieval castle with outer/inner curtain walls
- 9 outer towers + 4 inner towers with conical roofs
- Central keep with turret and corner towers
- 6 inner buildings (barracks, stables, chapel, storage, houses)
- Gatehouse with flanking pillars on south wall
- Procedural stone, roof, grass, bark, and leaves textures

### Combat
- Player has 100 HP (TCastleLiving on camera)
- Left-click attacks with sword swing animation (0.3s arc)
- Raycasts from crosshair center for hit detection
- Hit goblins take 25-40 damage per hit
- Goblins play "hurt" animation when hit, "die" animation when killed
- Dead goblins become non-collidable
- Player hurt flash (TCastleFlashEffect) when damaged
- Auto-revive on death

### Enemies
- 8 animated goblins placed around the terrain (outside castle walls)
- Hierarchical model with body parts (head, arms, legs, club weapon)
- 4 glTF animations: idle (body bob), move (leg swing), die (fall over), hurt (jolt)
- TCastleMoveAttack AI: chases player, attacks at close range
- Goblin stats: 50 HP, 5-10 damage, move speed 4, attack range 3

### NPCs and Dialog
- 2 NPCs at a campfire (X=15, Z=35, south of castle)
- Captain Aldric: gives "Kill 6 Goblins" quest
- Scout Mira: gives "Scout the River" quest
- Dialog system with choices (TViewDialog overlay with InterceptInput)
- Dialog changes based on quest state (pending/active/completed)

### Quest System
- Quest 0: "Slay the Raiders" - Kill 6 goblins (kill tracking)
- Quest 1: "Scout the River" - Walk near the river at X~80 (location check)
- Quest HUD label (top-left) shows active quest progress
- Notification popup on quest completion
- Kill quest completion: large "QUEST COMPLETED!" label + golden grail trophy spawns in front of player

### Mouse Look
- Starts ON by default
- Escape key turns it OFF
- Right-click toggles ON/OFF
- Status label at bottom of screen shows current state

## Key CGE Components Used

- `TCastleBackground` + `TCastleFog` in `$NonVisualComponents`
- `TCastleLiving` behavior for health/damage (on camera + goblins)
- `TCastleMoveAttack` behavior for goblin AI (chases + attacks player)
- `TCastleDamage` sub-component for attack damage config
- `TCastleFlashEffect` for player hurt visual feedback
- `TCastleCrosshair` for targeting reticle
- `TCastleSphere` (pmUnlit) for snow particles
- `TCastleCone` / `TCastleCylinder` / `TCastleBox` for sword model
- `TCastleScene.PlayAnimation()` for goblin hurt/die animations
- `Container.PushView` / `Container.PopView` for dialog overlay
- `Container.View` for title-to-game transition
- `MainViewport.TransformHit()` for combat raycasting
- `FindBehavior(TNpcBehavior)` for NPC detection
- `TCastleWalkNavigation.MouseLook` for first-person camera control

## GltfBuilder Animation Support

The `GltfBuilder` class in `generate_castle_glb.py` supports glTF 2.0 animations:
- `add_animation(name, channels_data)` - Create named animation with channels
- Each channel targets a node with translation/rotation/scale path
- Keyframe times and values packed as float accessors
- Animation data buffer views have no GPU target (not vertex/index data)
- `build_glb(root_nodes)` - Build with specific scene root nodes for hierarchical models

## Design File JSON Patterns

Background and Fog go in viewport's `$NonVisualComponents` and are referenced by name:
```json
"Background" : "WinterBackground",
"Fog" : "WinterFog",
"$NonVisualComponents" : [
  { "$$ClassName" : "TCastleBackground", "Name" : "WinterBackground", ... },
  { "$$ClassName" : "TCastleFog", "Name" : "WinterFog", ... }
]
```

Behaviors go in `$Behaviors` array on a transform/scene:
```json
"$Behaviors" : [
  { "$$ClassName" : "TCastleLiving", "Name" : "GoblinLiving1", "MaxLife" : 50.0 },
  { "$$ClassName" : "TCastleMoveAttack", "Enemy" : "PlayerLiving", "MoveSpeed" : 4.0, ... }
]
```
