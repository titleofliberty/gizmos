unit gizmoborderpanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TBorderPanel }

  TBorderPanel = class(TPanel)
  private
    FBorderColor: TColor;
    FBorderStyle: TPenStyle;
    FBorderRadius: Integer;
    FBorderWidth: Integer;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderStyle(AValue: TPenStyle);
    procedure SetBorderRadius(AValue: Integer);
    procedure SetBorderWidth(AValue: Integer);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  published
    property BorderWidth   : Integer read FBorderWidth write SetBorderWidth;
    property BorderRadius : Integer read FBorderRadius write SetBorderRadius;
    property BorderColor   : TColor read FBorderColor write SetBorderColor;
    property BorderStyle   : TPenStyle read FBorderStyle write SetBorderStyle;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I borderpanel_icon.lrs}
  RegisterComponents('Gizmos',[TBorderPanel]);
end;

{ TBorderPanel }

procedure TBorderPanel.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TBorderPanel.SetBorderStyle(AValue: TPenStyle);
begin
  if FBorderStyle = AValue then Exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TBorderPanel.SetBorderRadius(AValue: Integer);
begin
  if FBorderRadius = AValue then Exit;
  FBorderRadius := AValue;
  if FBorderRadius < 0 then FBorderRadius := 0;
  Invalidate;
end;

procedure TBorderPanel.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth = AValue then Exit;
  FBorderWidth := AValue;
  if (FBorderWidth < 0) then FBorderWidth := 0;
  Invalidate;
end;

constructor TBorderPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
end;

procedure TBorderPanel.Paint;
var
  rct : TRect;
begin
  inherited Paint;

  if FBorderWidth > 0 then
  begin
    rct := Rect(0, 0, Width, Height);
    rct.Inflate((FBorderWidth div 2) * -1, (FBorderWidth div 2) * -1);
    Canvas.Pen.Width   := FBorderWidth;
    Canvas.Pen.Style   := FBorderStyle;
    Canvas.Pen.Color   := FBorderColor;
    Canvas.Brush.Style := bsClear;

    if FBorderRadius > 0 then
      Canvas.RoundRect(rct, FBorderRadius, FBorderRadius)
    else
      Canvas.Rectangle(rct);
  end;
end;

end.
