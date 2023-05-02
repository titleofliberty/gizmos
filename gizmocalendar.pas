unit GizmoCalendar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DateUtils;

type

  { TGizmoCalendar }

  TGizmoCalendar = class(TCustomPanel)
  private
    FGrid : array [0..6, 0..7] of TRect;
    FDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  published
    property Value: TDateTime read FDate write SetDate;
    property Align;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I gizmocalendar_icon.lrs}
  RegisterComponents('Gizmos',[TGizmoCalendar]);
end;

{ TGizmoCalendar }

procedure TGizmoCalendar.SetDate(AValue: TDateTime);
begin
  if FDate = AValue then Exit;
  FDate := AValue;
end;

constructor TGizmoCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.FDate := Now();
  Self.Caption := '';
end;

procedure TGizmoCalendar.Paint;
var
  stepdate: TDateTime;
  x, y: integer;
  rowh, colw: integer;
  mth, yr: TRect;
  ts : TTextStyle;
begin
  inherited Paint;

  stepdate := IncDay(StartOfTheWeek(StartOfTheMonth(FDate)), -1);

  rowh := Self.Height div 8;
  colw := Self.Width div 7;

  Self.Canvas.Brush.Color := clDefault;
  Self.Canvas.FillRect(Self.BoundsRect);

  Self.Canvas.Font.Color := clWindowText;
  Self.Canvas.Pen.Color := clWindowText;
  Self.Canvas.Pen.Style := psSolid;
  for y := 0 to 7 do
    for x := 0 to 6 do
    begin
      FGrid[x,y] := TRect.Create(x * colw, y * rowh, (x * colw) + colw, (y * rowh) + rowh);
      Self.Canvas.Rectangle(grid[x,y]);
    end;

  ts.Alignment := taCenter;
  ts.Layout := tlCenter;

  mth := TRect.Create(FGrid[0,0].TopLeft, FGrid[1,0].BottomRight);
  yr := TRect.Create(FGrid[5,0].TopLeft, FGrid[6,0].BottomRight);

  Self.Canvas.TextRect(mth, 0, 0, FormatDateTime('mmmm', FDate), ts);

  Self.Canvas.TextRect(FGrid[0,1], 0, 0, 'Sun', ts);
  Self.Canvas.TextRect(FGrid[1,1], 0, 0, 'Mon', ts);
  Self.Canvas.TextRect(FGrid[2,1], 0, 0, 'Tue', ts);
  Self.Canvas.TextRect(FGrid[3,1], 0, 0, 'Wed', ts);
  Self.Canvas.TextRect(FGrid[4,1], 0, 0, 'Thu', ts);
  Self.Canvas.TextRect(FGrid[5,1], 0, 0, 'Fri', ts);
  Self.Canvas.TextRect(FGrid[6,1], 0, 0, 'Sat', ts);

  for y := 2 to 7 do
    for x := 0 to 6 do
    begin

      if (MonthOf(FDate) = MonthOf(stepdate)) then
        Self.Canvas.Font.Color := clWindowText
      else
        Self.Canvas.Font.Color := clGrayText;

      Self.Canvas.TextRect(FGrid[x,y], 0, 0, FormatDateTime('d', stepdate), ts);
      stepdate := IncDay(stepdate, 1);
    end;

end;


end.
