unit gizmosimplecard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  StdCtrls, ExtCtrls;

type

  { TCardBorder }

  TCardBorder = class(TPersistent)
  private
    FBorder: TColor;
    FOnChanged: TNotifyEvent;
    FRadius: Integer;
    FWidth: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetRadius(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
  published
    property Width: Integer read FWidth write SetWidth;
    property Radius: Integer read FRadius write SetRadius;
    property Color: TColor read FBorder write SetColor;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TCardLabel }

  TCardLabel = class(TPersistent)
  private
    FOnChanged : TNotifyEvent;
    FColor     : TColor;
    FAlignment : TAlignment;
    FCaption   : TCaption;
    FFont      : TFont;
    FHeight    : Integer;
    FLayout    : TTextLayout;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLayout(AValue: TTextLayout);
    procedure FontChanged(Sender: TObject);
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Layout: TTextLayout read FLayout write SetLayout;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write SetHeight;
    property Color: TColor read FColor write SetColor;
  end;

  { TGizmoSimpleCard }

  TGizmoSimpleCard = class(TPanel)
  private
    FBorder : TCardBorder;
    FHeader : TCardLabel;
    FFooter : TCardLabel;
    FBody   : TCardLabel;
    FPadding: Integer;
    procedure Paint; override;
    procedure SetBody(AValue: TCardLabel);
    procedure SetBorder(AValue: TCardBorder);
    procedure SetFooter(AValue: TCardLabel);
    procedure SetHeader(AValue: TCardLabel);
    procedure SetPadding(AValue: Integer);
    procedure LabelChanged(Sender: TObject);
    procedure BorderChanged(Sender: TObject);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Header  : TCardLabel  read FHeader  write SetHeader;
    property Footer  : TCardLabel  read FFooter  write SetFooter;
    property Body    : TCardLabel  read FBody    write SetBody;
    property Padding : Integer     read FPadding write SetPadding;
    property Border  : TCardBorder read FBorder  write SetBorder;
    property Anchors;
    property Align;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I gizmosimplecard_icon.lrs}
  RegisterComponents('Gizmos',[TGizmoSimpleCard]);
end;

{ TCardBorder }

procedure TCardBorder.SetColor(AValue: TColor);
begin
  if FBorder = AValue then Exit;
  FBorder := AValue;
end;

procedure TCardBorder.SetRadius(AValue: Integer);
begin
  if FRadius = AValue then Exit;
  if AValue < 0 then
    FRadius := 0
  else
    FRadius := AValue;
end;

procedure TCardBorder.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  if AValue < 0 then
    FWidth := 0
  else
    FWidth := AValue;
end;

constructor TCardBorder.Create;
begin
  inherited Create;

  Color := clWindowText;
end;

{ TCardLabel }

procedure TCardLabel.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.SetCaption(AValue: TCaption);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.SetFont(AValue: TFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.SetLayout(AValue: TTextLayout);
begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TCardLabel.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

constructor TCardLabel.Create;
begin
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;

  FColor     := clDefault;
  FCaption   := 'Sample Text';
  FAlignment := taLeftJustify;
  FLayout    := tlCenter;
  FHeight    := 48;
end;

{ TGizmoSimpleCard }

constructor TGizmoSimpleCard.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FBorder := TCardBorder.Create;

  Caption := '';
  FPadding := 4;
  BevelOuter := bvNone;

  FHeader := TCardLabel.Create;
  FFooter := TCardLabel.Create;
  FBody   := TCardLabel.Create;

  FHeader.OnChanged := @LabelChanged;
  FFooter.OnChanged := @LabelChanged;
  FBody.OnChanged   := @LabelChanged;
  FBorder.OnChanged := @BorderChanged;


  with Header do
  begin
    Caption := 'Header Text';
    Height := 48;
  end;

  with Footer do
  begin
    Caption := 'Footer Text';
    Height := 48;
  end;


  with Body do
  begin
    Caption := 'Body Text';
    Height := Height - (Header.Height + Footer.Height);
  end;

end;

procedure TGizmoSimpleCard.Paint;
var
  hdr, ftr, bdy, crd: TRect;
  ts : TTextStyle;
begin
  inherited Paint;

  ts.Clipping := true;
  ts.SingleLine := true;
  ts.SystemFont := false;
  ts.EndEllipsis := true;

  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(0, 0, Width, Height);

  crd := TRect.Create(Border.Width, Border.Width, Width, Height);
  InflateRect(crd, Border.Width * -2, Border.Width * -2);
  hdr := TRect.Create(crd.Left, crd.Top, crd.Right, Header.Height);
  ftr := TRect.Create(crd.Left, crd.Bottom - Footer.Height, crd.Right, crd.Bottom);
  bdy := TRect.Create(crd.Left, hdr.Bottom, crd.Right, ftr.Top);

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Header.Color;
  Canvas.FillRect(hdr);
  if (Header.Caption <> '') then
  begin
    hdr.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Header.Font;
    ts.Alignment := Header.Alignment;
    ts.Layout := Header.Layout;
    Canvas.TextRect(hdr, hdr.Left, hdr.Top, Header.Caption, ts);
  end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Footer.Color;
  Canvas.FillRect(ftr);
  if (Footer.Caption <> '') then
  begin
    ftr.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Footer.Font;
    ts.Alignment := Footer.Alignment;
    ts.Layout := Footer.Layout;
    Canvas.TextRect(ftr, ftr.Left, ftr.Top, Footer.Caption, ts);
  end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Body.Color;
  Canvas.FillRect(bdy);
  if (Body.Caption <> '') then
  begin
    bdy.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Body.Font;
    ts.Alignment := Body.Alignment;
    ts.Layout := Body.Layout;
    ts.Wordbreak := true;
    ts.SingleLine := false;
    Canvas.TextRect(bdy, bdy.Left, bdy.Top, Body.Caption, ts);
  end;

  if FBorder.Width > 0 then
  begin
    crd := TRect.Create(Border.Width, Border.Width, Width, Height);
    Canvas.Pen.Color := FBorder.Color;
    Canvas.Pen.Width := FBorder.Width;

    if FBorder.Radius > 0 then
      Canvas.RoundRect(crd, FBorder.Radius, FBorder.Radius)
    else
      Canvas.Rectangle(crd);
  end;

end;

procedure TGizmoSimpleCard.SetBody(AValue: TCardLabel);
begin
  if FBody = AValue then Exit;
  FBody := AValue;
end;

procedure TGizmoSimpleCard.SetBorder(AValue: TCardBorder);
begin
  if FBorder = AValue then Exit;
  FBorder := AValue;
  Invalidate;
end;

procedure TGizmoSimpleCard.SetFooter(AValue: TCardLabel);
begin
  if FFooter = AValue then Exit;
  FFooter := AValue;
end;

procedure TGizmoSimpleCard.SetHeader(AValue: TCardLabel);
begin
  if FHeader = AValue then Exit;
  FHeader := AValue;
end;

procedure TGizmoSimpleCard.SetPadding(AValue: Integer);
begin
  if FPadding = AValue then Exit;
  FPadding := AValue;
  Invalidate;
end;

procedure TGizmoSimpleCard.LabelChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TGizmoSimpleCard.BorderChanged(Sender: TObject);
begin
  Invalidate;
end;

end.
