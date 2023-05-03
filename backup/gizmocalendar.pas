unit GizmoCalendar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DateUtils, Types;

type

  { TGizmoCalendar }

  TGizmoCalendar = class(TCustomPanel)
  private
    FMousePos : TPoint;
    FMthPrev, FMthNext: TRect;
    FGrid : array [0..6, 0..7] of TRect;
    FDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    property Value: TDateTime read FDate write SetDate;
    property Align;
    property Anchors;
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
  Self.Canvas.FillRect(0, 0, Width, Height);
  Self.Canvas.Brush.Style := bsClear;

  Self.Canvas.Font.Color := clWindowText;
  Self.Canvas.Pen.Color := clWindowText;
  Self.Canvas.Pen.Style := psSolid;
  for y := 0 to 7 do
    for x := 0 to 6 do
    begin
      FGrid[x,y] := TRect.Create(x * colw, y * rowh, (x * colw) + colw, (y * rowh) + rowh);
    end;

  ts.Alignment := taCenter;
  ts.Layout := tlCenter;

  mth := TRect.Create(FGrid[0,0].TopLeft, FGrid[1,0].BottomRight);
  yr := TRect.Create(FGrid[5,0].TopLeft, FGrid[6,0].BottomRight);
  FMthPrev := TRect.Create(mth.Left, mth.Top, 7, 14);
  FMthNext := TRect.Create(mth.Right - 7, mth.Top, mth.Right, 14);
  FMthPrev.Offset(0, (mth.Height - FMthPrev.Height) div 2);
  FMthNext.Offset(0, (mth.Height - FMthNext.Height) div 2);

  Self.Canvas.TextRect(mth, 0, 0, FormatDateTime('mmmm', FDate), ts);
  Self.Canvas.TextRect(yr, 0, 0, FormatDateTime('yyyy', FDate), ts);

  Self.Canvas.Brush.Color := clWindowText;
  Self.Canvas.Brush.Style := bsSolid;
  Self.Canvas.Polygon([Point(0,FMthPrev.CenterPoint.Y), Point(FMthPrev.Right, FMthPrev.Top), FMthPrev.BottomRight]);
  Self.Canvas.Polygon([FMthNext.TopLeft, Point(FMthNext.Right, FMthNext.Top + (FMthNext.Height div 2)), Point(FMthNext.Left, FMthNext.Bottom)]);
  Self.Canvas.Brush.Color := clDefault;
  Self.Canvas.Brush.Style := bsClear;


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

      if PtInRect(FGrid[x,y], FMousePos) then
      begin
        Self.Canvas.Brush.Style := bsSolid;
        Self.Canvas.Brush.Color := clHighlight;
        Self.Canvas.FillRect(FGrid[x,y]);
        Self.Canvas.Font.Color := clHighlightText;
        Self.Canvas.Brush.Color := clDefault;
        Self.Canvas.Brush.Style := bsClear;
      end
      else if (MonthOf(FDate) = MonthOf(stepdate)) then
        Self.Canvas.Font.Color := clWindowText
      else
        Self.Canvas.Font.Color := clGrayText;

      Self.Canvas.TextRect(FGrid[x,y], 0, 0, FormatDateTime('d', stepdate), ts);
      stepdate := IncDay(stepdate, 1);
    end;

end;

procedure TGizmoCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FMousePos := TPoint.Create(X, Y);
  Invalidate;
end;

procedure TGizmoCalendar.MouseLeave;
begin
  FMousePos := TPoint.Create(0, 0);
  Invalidate;

  inherited MouseLeave;
end;

procedure TGizmoCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if PtInRect(FMthPrev, FMousePos) then
    FDate := IncMonth(FDate, -1)
  else if PtInRect(FMthNext, FMousePos) then
    FDate := IncMonth(FDate, 1);

  Invalidate;
end;


end.
