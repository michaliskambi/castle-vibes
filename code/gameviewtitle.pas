{ Title screen view - "Castle Vibes" }
unit GameViewTitle;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleColors;

type
  TViewTitle = class(TCastleView)
  private
    procedure ClickStart(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  ViewTitle: TViewTitle;

implementation

uses GameViewMain;

procedure TViewTitle.Start;
var
  Background: TCastleRectangleControl;
  TitleLabel: TCastleLabel;
  SubtitleLabel: TCastleLabel;
  StartButton: TCastleButton;
begin
  inherited;

  { Dark background }
  Background := TCastleRectangleControl.Create(FreeAtStop);
  Background.Color := Vector4(0.05, 0.05, 0.12, 1.0);
  Background.FullSize := true;
  InsertFront(Background);

  { Title: "Castle Vibes" }
  TitleLabel := TCastleLabel.Create(FreeAtStop);
  TitleLabel.Caption := 'Castle Vibes';
  TitleLabel.Color := Vector4(1.0, 0.85, 0.0, 1.0);
  TitleLabel.FontSize := 72;
  TitleLabel.Anchor(hpMiddle);
  TitleLabel.Anchor(vpTop, -120);
  Background.InsertFront(TitleLabel);

  { Subtitle }
  SubtitleLabel := TCastleLabel.Create(FreeAtStop);
  SubtitleLabel.Caption := 'The Winter Siege';
  SubtitleLabel.Color := White;
  SubtitleLabel.FontSize := 28;
  SubtitleLabel.Anchor(hpMiddle);
  SubtitleLabel.Anchor(vpTop, -210);
  Background.InsertFront(SubtitleLabel);

  { Start button }
  StartButton := TCastleButton.Create(FreeAtStop);
  StartButton.Caption := 'Start Game';
  StartButton.FontSize := 24;
  StartButton.PaddingHorizontal := 40;
  StartButton.PaddingVertical := 15;
  StartButton.Anchor(hpMiddle);
  StartButton.Anchor(vpMiddle, -30);
  StartButton.OnClick := {$ifdef FPC}@{$endif} ClickStart;
  Background.InsertFront(StartButton);
end;

procedure TViewTitle.ClickStart(Sender: TObject);
begin
  Container.View := ViewMain;
end;

end.
