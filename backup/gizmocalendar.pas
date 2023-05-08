unit GizmoCalendar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DateUtils, Types;

type

  TDatesArray = array of TDateTime;

  TArrowDirection = (adLeft, adRight);

  TCell = record
    Date : TDateTime;
    Rect : TRect;
  end;

  { TGizmoCalendar }

  TGizmoCalendar = class(TCustomPanel)
  private
    FDates: TDatesArray;
    FMousePos : TPoint;
    FMthPrev, FMthNext: TRect;
    FOnChange: TNotifyEvent;
    FYrPrev, FYrNext: TRect;
    FGrid : array [0..6, 0..7] of TCell;
    FDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
    procedure RenderArrow(ARect: TRect; ADirection: TArrowDirection);
    procedure SetDates(AValue: TDatesArray);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property HighlightDates: TDatesArray read FDates write SetDates;
  published
    property Font;
    property Align;
    property Anchors;
    property Value: TDateTime read FDate write SetDate;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
  if Assigned(FOnChange) then FOnChange(Self);
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

procedure TGizmoCalendar.SetDates(AValue: TDatesArray);
begin
  if FDates = AValue then Exit;
  FDates := AValue;
  Invalidate;
end;

constructor TGizmoCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Width  := 300;
  Height := 300;

  SetDate(Now());
  Self.Caption := '';
end;

procedure TGizmoCalendar.Paint;
var
  stepdate: TDateTime;
  x, y, i: integer;
  rowh, colw: integer;
  mth, yr, hl: TRect;
  ts : TTextStyle;
begin
  inherited Paint;

  Canvas.Font.Name := Self.Font.Name;
  Canvas.Font.Size := Self.Font.Size;

  stepdate := StartOfTheWeek(StartOfTheMonth(FDate));
  if DayOfWeek(stepdate) < DateUtils.DaySunday then
    stepdate := IncDay(stepdate, -1);

  rowh := Self.Height div 8;
  colw := Self.Width div 7;

  Canvas.Brush.Color := clDefault;
  Canvas.FillRect(0, 0, Width, Height);
  Canvas.Brush.Style := bsClear;

  Canvas.Font.Color := clWindowText;
  Canvas.Pen.Color := clWindowText;
  Canvas.Pen.Style := psSolid;
  for y := 0 to 7 do
    for x := 0 to 6 do
    begin
      FGrid[x,y].Rect := TRect.Create(x * colw, y * rowh, (x * colw) + colw, (y * rowh) + rowh);
    end;

  ts.Alignment := taCenter;
  ts.Layout := tlCenter;
  ts.SystemFont := false;

  mth := TRect.Union(FGrid[0,0].Rect, FGrid[1,0].Rect);
  yr := TRect.Union(FGrid[5,0].Rect, FGrid[6,0].Rect);

  FMthPrev := TRect.Create(mth.Left, mth.Top, 7, 14);
  FMthNext := TRect.Create(mth.Right - 7, mth.Top, mth.Right, 14);
  FYrPrev := TRect.Create(yr.Left, yr.Top, yr.Left + 7, yr.Top + 14);
  FYrNext := TRect.Create(yr.Right - 7, yr.Top, yr.Right, yr.Top + 14);

  FMthPrev.Offset(0, (mth.Height - FMthPrev.Height) div 2);
  FMthNext.Offset(0, (mth.Height - FMthNext.Height) div 2);
  FYrPrev.Offset(0, (yr.Height - FYrPrev.Height) div 2);
  FYrNext.Offset(0, (yr.Height - FYrNext.Height) div 2);

  RenderArrow(FMthPrev, adLeft);
  RenderArrow(FMthNext, adRight);
  RenderArrow(FYrPrev, adLeft);
  RenderArrow(FYrNext, adRight);

  Canvas.TextRect(mth, mth.Left, mth.Top, FormatDateTime('mmmm', FDate), ts);
  Canvas.TextRect(yr, yr.Left, yr.Top, FormatDateTime('yyyy', FDate), ts);

  Canvas.TextRect(FGrid[0,1].Rect, FGrid[0,1].Rect.Left, FGrid[0,1].Rect.Top, 'Sun', ts);
  Canvas.TextRect(FGrid[1,1].Rect, FGrid[1,1].Rect.Left, FGrid[1,1].Rect.Top, 'Mon', ts);
  Canvas.TextRect(FGrid[2,1].Rect, FGrid[2,1].Rect.Left, FGrid[2,1].Rect.Top, 'Tue', ts);
  Canvas.TextRect(FGrid[3,1].Rect, FGrid[3,1].Rect.Left, FGrid[3,1].Rect.Top, 'Wed', ts);
  Canvas.TextRect(FGrid[4,1].Rect, FGrid[4,1].Rect.Left, FGrid[4,1].Rect.Top, 'Thu', ts);
  Canvas.TextRect(FGrid[5,1].Rect, FGrid[5,1].Rect.Left, FGrid[5,1].Rect.Top, 'Fri', ts);
  Canvas.TextRect(FGrid[6,1].Rect, FGrid[6,1].Rect.Left, FGrid[6,1].Rect.Top, 'Sat', ts);

  for y := 2 to 7 do
    for x := 0 to 6 do
    begin
      FGrid[x,y].Date := stepdate;

      if PtInRect(FGrid[x,y].Rect, FMousePos) then
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clHighlight;
        Canvas.FillRect(FGrid[x,y].Rect);
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clDefault;
        Canvas.Brush.Style := bsClear;
      end
      else if (MonthOf(FDate) = MonthOf(stepdate)) then
        Canvas.Font.Color := clWindowText
      else
        Canvas.Font.Color := clGrayText;

      for i := low(FDates) to high(FDates) do
        if (CompareDate(FGrid[x,y].Date, FDates[i]) = 0) then
        begin
          hl := TRect.Create(FGrid[x,y].Rect);
          hl.Inflate(-3, -3);
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := clHighlight;
          Canvas.FillRect(hl);
          Canvas.Font.Color := clHighlightText;
          Canvas.Brush.Color := clDefault;
          Canvas.Brush.Style := bsClear;
        end;

      if (CompareDate(FDate, FGrid[x,y].Date) = 0) then
        Canvas.Rectangle(FGrid[x,y].Rect);

      Canvas.TextRect(FGrid[x,y].Rect, FGrid[x,y].Rect.Left, FGrid[x,y].Rect.Top, FormatDateTime('d', stepdate), ts);
      stepdate := IncDay(stepdate, 1);
    end;

end;

procedure TGizmoCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FMousePos := TPoint.Create(X, Y);

  if PtInRect(FMthPrev, FMousePos) then
    Cursor := crHandPoint
  else if PtInRect(FMthNext, FMousePos) then
    Cursor := crHandPoint
  else if PtInRect(FYrPrev, FMousePos) then
    Cursor := crHandPoint
  else if PtInRect(FYrNext, FMousePos) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;

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
