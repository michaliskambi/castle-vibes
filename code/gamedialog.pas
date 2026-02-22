{ Dialog tree data for NPCs. }
unit GameDialog;

interface

type
  TQuestAction = (qaNone, qaGiveQuest0, qaGiveQuest1, qaCompleteQuest0, qaCompleteQuest1);

  TDialogChoice = record
    ChoiceText: String;
    NextDialogId: Integer;   { -1 = close dialog }
    QuestAction: TQuestAction;
  end;

  TDialogEntry = record
    Speaker: String;
    Text: String;
    Choices: array of TDialogChoice;
  end;

var
  Dialogs: array of TDialogEntry;

procedure InitDialogs;

implementation

procedure InitDialogs;
begin
  SetLength(Dialogs, 8);

  { Dialog 0: Captain Aldric - initial greeting }
  Dialogs[0].Speaker := 'Captain Aldric';
  Dialogs[0].Text := 'Hail, traveler! You''ve arrived at a dark hour. ' +
    'Goblin raiders have been attacking our outpost for days. ' +
    'Our garrison is weakened and we need help.';
  SetLength(Dialogs[0].Choices, 2);
  Dialogs[0].Choices[0].ChoiceText := 'I''ll help. What do you need?';
  Dialogs[0].Choices[0].NextDialogId := 1;
  Dialogs[0].Choices[0].QuestAction := qaNone;
  Dialogs[0].Choices[1].ChoiceText := 'I''m just passing through.';
  Dialogs[0].Choices[1].NextDialogId := -1;
  Dialogs[0].Choices[1].QuestAction := qaNone;

  { Dialog 1: Captain Aldric - gives kill quest }
  Dialogs[1].Speaker := 'Captain Aldric';
  Dialogs[1].Text := 'The goblins raid in small groups. ' +
    'Kill 6 of them and their war band will scatter. ' +
    'They roam the fields around the castle. Be careful, they fight back!';
  SetLength(Dialogs[1].Choices, 1);
  Dialogs[1].Choices[0].ChoiceText := 'Consider it done.';
  Dialogs[1].Choices[0].NextDialogId := -1;
  Dialogs[1].Choices[0].QuestAction := qaGiveQuest0;

  { Dialog 2: Captain Aldric - quest active }
  Dialogs[2].Speaker := 'Captain Aldric';
  Dialogs[2].Text := 'Have you driven off those goblins yet? ' +
    'We can hear them skulking around the castle walls at night.';
  SetLength(Dialogs[2].Choices, 1);
  Dialogs[2].Choices[0].ChoiceText := 'I''m working on it.';
  Dialogs[2].Choices[0].NextDialogId := -1;
  Dialogs[2].Choices[0].QuestAction := qaNone;

  { Dialog 3: Captain Aldric - quest complete }
  Dialogs[3].Speaker := 'Captain Aldric';
  Dialogs[3].Text := 'You''ve done it! The goblin raiding party has scattered. ' +
    'The outpost is safe, at least for now. You have our deepest gratitude, warrior.';
  SetLength(Dialogs[3].Choices, 1);
  Dialogs[3].Choices[0].ChoiceText := 'Glad I could help.';
  Dialogs[3].Choices[0].NextDialogId := -1;
  Dialogs[3].Choices[0].QuestAction := qaNone;

  { Dialog 4: Scout Mira - initial greeting }
  Dialogs[4].Speaker := 'Scout Mira';
  Dialogs[4].Text := 'Hey! I''m Mira, the garrison scout. ' +
    'I''ve spotted a goblin camp near the river to the east, ' +
    'but it''s too dangerous for me to go alone.';
  SetLength(Dialogs[4].Choices, 2);
  Dialogs[4].Choices[0].ChoiceText := 'I''ll scout it out for you.';
  Dialogs[4].Choices[0].NextDialogId := 5;
  Dialogs[4].Choices[0].QuestAction := qaNone;
  Dialogs[4].Choices[1].ChoiceText := 'Maybe later.';
  Dialogs[4].Choices[1].NextDialogId := -1;
  Dialogs[4].Choices[1].QuestAction := qaNone;

  { Dialog 5: Scout Mira - gives scout quest }
  Dialogs[5].Speaker := 'Scout Mira';
  Dialogs[5].Text := 'Head east until you reach the river. ' +
    'Just getting close enough to observe should tell us what we need to know. ' +
    'Report back when you''ve seen their camp.';
  SetLength(Dialogs[5].Choices, 1);
  Dialogs[5].Choices[0].ChoiceText := 'I''ll head east.';
  Dialogs[5].Choices[0].NextDialogId := -1;
  Dialogs[5].Choices[0].QuestAction := qaGiveQuest1;

  { Dialog 6: Scout Mira - quest active }
  Dialogs[6].Speaker := 'Scout Mira';
  Dialogs[6].Text := 'The river is to the east. ' +
    'Just get close enough to observe and come back.';
  SetLength(Dialogs[6].Choices, 1);
  Dialogs[6].Choices[0].ChoiceText := 'On my way.';
  Dialogs[6].Choices[0].NextDialogId := -1;
  Dialogs[6].Choices[0].QuestAction := qaNone;

  { Dialog 7: Scout Mira - quest complete }
  Dialogs[7].Speaker := 'Scout Mira';
  Dialogs[7].Text := 'You made it back! ' +
    'Now we know where they''re camped. This information is invaluable. Thank you!';
  SetLength(Dialogs[7].Choices, 1);
  Dialogs[7].Choices[0].ChoiceText := 'Happy to help.';
  Dialogs[7].Choices[0].NextDialogId := -1;
  Dialogs[7].Choices[0].QuestAction := qaNone;
end;

end.
