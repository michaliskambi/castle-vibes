{ NPC behavior for talkable characters. }
unit GameNpcs;

interface

uses CastleTransform;

type
  TNpcBehavior = class(TCastleBehavior)
  public
    NpcName: String;
    DialogId: Integer;
  end;

implementation

end.
