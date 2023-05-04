unit GizmoCalendar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DateUtils, Types;

type

  TArrowDirection = (adLeft, adRight);

  TCell = record
    Date : TDateTime;
    Rect : TRect;
  end;

  { TGizmoCalendar }

  TGizmoCalendar = class(TCustomPanel)
  private
    FMousePos : TPoint;
    FMthPrev, FMthNext: TRect;
    FYrPrev, FYrNext: TRect;
    FGrid : array [0..6, 0..7] of TCell;
    FDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
    procedure RenderArrow(ARect: TRect; ADirection: TArrowDirection);
    procedure TextCentered(ARect: TRect; AText: String);
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
    property Font;
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
  Invalidate;
end;

procedure TGizmoCalendar.RenderArrow(ARect: TRect; ADirection: TArrowDirection);
begin

  Self.Canvas.Brush.Color := clWindowText;
  Self.Canvas.Brush.Style := bsSolid;

  if (ADirection = adLeft) then
    Self.Canvas.Polygon([Point(ARect.Left, ARect.CenterPoint.Y), Point(ARect.Right, ARect.Top), ARect.BottomRight])
  else if (ADirection = adRight) then
    Self.Canvas.Polygon([ARect.TopLeft, Point(ARect.Right, ARect.Top + (ARect.Height div 2)), Point(ARect.Left, ARect.Bottom)]);

  Self.Canvas.Brush.Color := clDefault;
  Self.Canvas.Brush.Style := bsClear;

end;

procedure TGizmoCalendar.TextCentered(ARect: TRect; AText: String);
var
  w, h: Integer;
begin
  w := Self.Canvas.TextWidth(AText);
  h := Self.Canvas.TextHeight(AText);
  Self.Canvas.TextOut(ARect.Left + ((ARect.Width-w) div 2), ARect.Top + ((ARect.Height-h) div 2), AText);
end;

constructor TGizmoCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetDate(Now());
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

  Self.Canvas.Font.Name := Self.Font.Name;
  Self.Canvas.Font.Size := Self.Font.Size;

  stepdate := StartOfTheWeek(StartOfTheMonth(FDate));
  if DayOfWeek(stepdate) < DateUtils.DaySunday then
    stepdate := IncDay(stepdate, -1);

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
      FGrid[x,y].Rect := TRect.Create(x * colw, y * rowh, (x * colw) + colw, (y * rowh) + rowh);
    end;

  ts.Alignment := taCenter;
  ts.Layout := tlCenter;

  mth := TRect.Union(FGrid[0,0].Rect, FGrid[1,0].Rect);
  yr := TRect.Union(FGrid[5,0].Rect, FGrid[6,0].Rect);

  FMthPrev := TRect.Create(mth.Left, mth.Top, 7, 14);
  FMthNext := TRect.Create(mth.Right - 7, mth.Top, mth.Right, 14);
  FYrPrev := TRect.Create(yr.Left, yr.Top, 7, 14);
  FYrNext := TRect.Create(yr.Right - 7, yr.Top, yr.Right, 14);

  FMthPrev.Offset(0, (mth.Height - FMthPrev.Height) div 2);
  FMthNext.Offset(0, (mth.Height - FMthNext.Height) div 2);
  FYrPrev.Offset(0, (yr.Height - FYrPrev.Height) div 2);
  FYrNext.Offset(0, (yr.Height - FYrNext.Height) div 2);

  RenderArrow(FMthPrev, adLeft);
  RenderArrow(FMthNext, adRight);
  RenderArrow(FYrPrev, adLeft);
  RenderArrow(FYrNext, adRight);


  TextCentered(mth, FormatDateTime('mmmm', FDate));
  TextCentered(yr, FormatDateTime('yyyy', FDate));

  TextCentered(FGrid[0,1].Rect, 'Sun');
  TextCentered(FGrid[1,1].Rect, 'Mon');
  TextCentered(FGrid[2,1].Rect, 'Tue');
  TextCentered(FGrid[3,1].Rect, 'Wed');
  TextCentered(FGrid[4,1].Rect, 'Thu');
  TextCentered(FGrid[5,1].Rect, 'Fri');
  TextCentered(FGrid[6,1].Rect, 'Sat');

  for y := 2 to 7 do
    for x := 0 to 6 do
    begin
      FGrid[x,y].Date := stepdate;

      if PtInRect(FGrid[x,y].Rect, FMousePos) then
      begin
        Self.Canvas.Brush.Style := bsSolid;
        Self.Canvas.Brush.Color := clHighlight;
        Self.Canvas.FillRect(FGrid[x,y].Rect);
        Self.Canvas.Font.Color := clHighlightText;
        Self.Canvas.Brush.Color := clDefault;
        Self.Canvas.Brush.Style := bsClear;
      end
      else if (MonthOf(FDate) = MonthOf(stepdate)) then
        Self.Canvas.Font.Color := clWindowText
      else
        Self.Canvas.Font.Color := clGrayText;

      if (CompareDate(FDate, FGrid[x,y].Date) = 0) then
        Self.Canvas.Rectangle(FGrid[x,y].Rect);
      TextCentered(FGrid[x,y].Rect, FormatDateTime('d', stepdate));
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
var
  xx, yy: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if PtInRect(FMthPrev, FMousePos) then
    SetDate(IncMonth(FDate, -1))
  else if PtInRect(FMthNext, FMousePos) then
    SetDate(IncMonth(FDate, 1))
  else if PtInRect(FYrPrev, FMousePos) then
    SetDate(IncYear(FDate, -1))
  else if PtInRect(FYrNext, FMousePos) then
    SetDate(IncYear(FDate, 1));

  for yy := 2 to 7 do
    for xx := 0 to 6 do
      if PtInRect(FGrid[xx,yy].Rect, FMousePos) then
        SetDate(FGrid[xx,yy].Date);
end;


end.
