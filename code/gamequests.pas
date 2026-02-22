{ Quest tracking system for "The Winter Siege". }
unit GameQuests;

interface

uses SysUtils, CastleVectors;

type
  TQuestType = (qtKill, qtScout);
  TQuestState = (qsPending, qsActive, qsCompleted);

  TQuest = record
    Name: String;
    Description: String;
    QuestType: TQuestType;
    TargetCount: Integer;
    CurrentCount: Integer;
    State: TQuestState;
  end;

  TQuestManager = class
  private
    FQuests: array [0..1] of TQuest;
    FLastCompletedQuest: Integer;
  public
    constructor Create;
    procedure ActivateQuest(Id: Integer);
    procedure AddKill;
    procedure CheckScoutLocation(const PlayerPos: TVector3);
    function GetActiveQuestText: String;
    function GetQuest(Id: Integer): TQuest;
    function LastCompletedQuest: Integer;
    procedure ClearLastCompleted;
  end;

var
  QuestManager: TQuestManager;

implementation

constructor TQuestManager.Create;
begin
  inherited;
  FLastCompletedQuest := -1;

  FQuests[0].Name := 'Slay the Raiders';
  FQuests[0].Description := 'Kill 6 goblins to break the siege.';
  FQuests[0].QuestType := qtKill;
  FQuests[0].TargetCount := 6;
  FQuests[0].CurrentCount := 0;
  FQuests[0].State := qsPending;

  FQuests[1].Name := 'Scout the River';
  FQuests[1].Description := 'Scout the goblin camp near the river to the east.';
  FQuests[1].QuestType := qtScout;
  FQuests[1].TargetCount := 1;
  FQuests[1].CurrentCount := 0;
  FQuests[1].State := qsPending;
end;

procedure TQuestManager.AddKill;
begin
  { Always count kills, even if quest not yet given }
  if FQuests[0].State in [qsActive, qsPending] then
  begin
    Inc(FQuests[0].CurrentCount);
    if (FQuests[0].State = qsActive) and
       (FQuests[0].CurrentCount >= FQuests[0].TargetCount) then
    begin
      FQuests[0].State := qsCompleted;
      FLastCompletedQuest := 0;
    end;
  end;
end;

procedure TQuestManager.ActivateQuest(Id: Integer);
begin
  if (Id >= 0) and (Id <= High(FQuests)) and (FQuests[Id].State = qsPending) then
  begin
    FQuests[Id].State := qsActive;
    { Check if already completed (kills counted before quest given) }
    if (Id = 0) and (FQuests[0].CurrentCount >= FQuests[0].TargetCount) then
    begin
      FQuests[0].State := qsCompleted;
      FLastCompletedQuest := 0;
    end;
  end;
end;

procedure TQuestManager.CheckScoutLocation(const PlayerPos: TVector3);
begin
  if FQuests[1].State = qsActive then
  begin
    { River is at X ~= 80, check if player reached near it }
    if (PlayerPos.X > 65) and (PlayerPos.X < 95) then
    begin
      FQuests[1].CurrentCount := 1;
      FQuests[1].State := qsCompleted;
      FLastCompletedQuest := 1;
    end;
  end;
end;

function TQuestManager.GetActiveQuestText: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(FQuests) do
  begin
    if FQuests[I].State = qsActive then
    begin
      if FQuests[I].QuestType = qtKill then
        Result := Result + FQuests[I].Name + ': ' +
          IntToStr(FQuests[I].CurrentCount) + '/' +
          IntToStr(FQuests[I].TargetCount) + LineEnding
      else
        Result := Result + FQuests[I].Name + LineEnding;
    end;
  end;
  if Result = '' then
  begin
    { Check if all completed }
    if (FQuests[0].State = qsCompleted) and (FQuests[1].State = qsCompleted) then
      Result := 'All quests completed! The siege is broken!'
    else
      Result := 'Talk to the NPCs at the campfire.';
  end;
end;

function TQuestManager.GetQuest(Id: Integer): TQuest;
begin
  Result := FQuests[Id];
end;

function TQuestManager.LastCompletedQuest: Integer;
begin
  Result := FLastCompletedQuest;
end;

procedure TQuestManager.ClearLastCompleted;
begin
  FLastCompletedQuest := -1;
end;

end.
