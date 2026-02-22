{ Dialog overlay view for NPC conversations. }
unit GameViewDialog;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleColors;

type
  TViewDialog = class(TCastleView)
  private
    Background: TCastleRectangleControl;
    LabelSpeaker: TCastleLabel;
    LabelMessage: TCastleLabel;
    ChoiceButtons: array [0..3] of TCastleButton;
    ChoiceNextDialogIds: array [0..3] of Integer;
    ChoiceQuestActions: array [0..3] of Integer;
    NumChoices: Integer;
    procedure ClickChoice(Sender: TObject);
  public
    CurrentDialogId: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewDialog: TViewDialog;

implementation

uses SysUtils, GameDialog, GameQuests, GameViewMain;

constructor TViewDialog.Create(AOwner: TComponent);
begin
  inherited;
  InterceptInput := true;
end;

procedure TViewDialog.Start;
var
  I: Integer;
  Dialog: TDialogEntry;
  VertGroup: TCastleVerticalGroup;
begin
  inherited;

  { Disable mouse look so cursor is free to click buttons }
  if ViewMain.WalkNavigation <> nil then
    ViewMain.WalkNavigation.MouseLook := false;

  if (CurrentDialogId < 0) or (CurrentDialogId >= Length(Dialogs)) then
  begin
    Container.PopView(Self);
    Exit;
  end;

  Dialog := Dialogs[CurrentDialogId];

  { Semi-transparent dark background }
  Background := TCastleRectangleControl.Create(FreeAtStop);
  Background.Color := Vector4(0, 0, 0, 0.75);
  Background.FullSize := true;
  InsertFront(Background);

  { Centered content panel }
  VertGroup := TCastleVerticalGroup.Create(FreeAtStop);
  VertGroup.Spacing := 15;
  VertGroup.Alignment := hpMiddle;
  VertGroup.Anchor(hpMiddle);
  VertGroup.Anchor(vpMiddle);
  Background.InsertFront(VertGroup);

  { Speaker name }
  LabelSpeaker := TCastleLabel.Create(FreeAtStop);
  LabelSpeaker.Caption := Dialog.Speaker;
  LabelSpeaker.Color := Vector4(1, 0.85, 0.3, 1);
  LabelSpeaker.FontSize := 28;
  LabelSpeaker.Alignment := hpMiddle;
  VertGroup.InsertFront(LabelSpeaker);

  { Message text }
  LabelMessage := TCastleLabel.Create(FreeAtStop);
  LabelMessage.Caption := Dialog.Text;
  LabelMessage.Color := White;
  LabelMessage.FontSize := 22;
  LabelMessage.MaxWidth := 600;
  LabelMessage.Alignment := hpMiddle;
  VertGroup.InsertFront(LabelMessage);

  { Choice buttons }
  NumChoices := Length(Dialog.Choices);
  if NumChoices > 4 then
    NumChoices := 4;

  for I := 0 to NumChoices - 1 do
  begin
    ChoiceButtons[I] := TCastleButton.Create(FreeAtStop);
    ChoiceButtons[I].Caption := Dialog.Choices[I].ChoiceText;
    ChoiceButtons[I].FontSize := 20;
    ChoiceButtons[I].PaddingHorizontal := 20;
    ChoiceButtons[I].PaddingVertical := 10;
    ChoiceButtons[I].Tag := I;
    ChoiceButtons[I].OnClick := {$ifdef FPC}@{$endif} ClickChoice;
    ChoiceNextDialogIds[I] := Dialog.Choices[I].NextDialogId;
    ChoiceQuestActions[I] := Ord(Dialog.Choices[I].QuestAction);
    VertGroup.InsertFront(ChoiceButtons[I]);
  end;
end;

procedure TViewDialog.Stop;
begin
  { Restore mouse look when dialog fully closes.
    When navigating between dialogs (Pop+Push), Start will disable it again. }
  if (ViewMain <> nil) and (ViewMain.WalkNavigation <> nil) then
    ViewMain.WalkNavigation.MouseLook := ViewMain.MouseLookActive;
  inherited;
end;

procedure TViewDialog.ClickChoice(Sender: TObject);
var
  Btn: TCastleButton;
  ChoiceIdx, NextId, Action: Integer;
begin
  Btn := Sender as TCastleButton;
  ChoiceIdx := Btn.Tag;
  NextId := ChoiceNextDialogIds[ChoiceIdx];
  Action := ChoiceQuestActions[ChoiceIdx];

  { Execute quest action }
  case TQuestAction(Action) of
    qaGiveQuest0: QuestManager.ActivateQuest(0);
    qaGiveQuest1: QuestManager.ActivateQuest(1);
    else { qaNone, qaCompleteQuest - no action needed here };
  end;

  { Navigate to next dialog or close }
  if NextId >= 0 then
  begin
    CurrentDialogId := NextId;
    Container.PopView(Self);
    Container.PushView(Self);
  end
  else
    Container.PopView(Self);
end;

function TViewDialog.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

  if Event.IsKey(keyEscape) then
  begin
    Container.PopView(Self);
    Exit(true);
  end;
end;

end.
